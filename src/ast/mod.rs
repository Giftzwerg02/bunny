#![allow(dead_code)]

pub mod expressions;
pub mod symbol_table;

use std::fmt::{Debug, Display};
use text_trees::StringTreeNode;

use expressions::{Argument, Color, FuncCall, NamedArgument, NonEmptyFuncCall, Symbol};
use pest::iterators::Pair;

use crate::parser::Rule;

pub trait Expr: Clone + Debug + PartialEq + Display {
    fn pretty_print(&self) -> StringTreeNode;
}

#[derive(Clone, Debug, PartialEq)]
pub enum AST<TExpr: Expr> {
    Int(u64),
    Float(f64),
    String(String),
    Color(Color),
    Symbol(Symbol),
    FuncCall(FuncCall<TExpr>),
    Argument(Argument<TExpr>),
    Array(Vec<TExpr>),
    Dict(Vec<(TExpr, TExpr)>),
}

impl<TExpr: Expr> Display for AST<TExpr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AST::Int(int) => write!(f, "{int}"),
            AST::Float(float) => {
                write!(f, "{float}f")
            }
            AST::String(s) => write!(f, "\"{s}\""),
            AST::Array(exprs) => {
                let str = exprs.iter().map(|e| format!("{e}")).collect::<Vec<_>>();
                let str = str.join(" ");
                write!(f, "[ {str} ]")
            }
            AST::Dict(items) => {
                if items.len() == 0 {
                    return write!(f, "[:]");
                }
                let str = items
                    .iter()
                    .map(|(k, v)| format!("{k} : {v}"))
                    .collect::<Vec<_>>();
                let str = str.join(" ");
                write!(f, "[ {str} ]")
            }
            AST::FuncCall(func_call) => {
                write!(f, "{func_call}")
            }
            AST::Color(color) => {
                write!(f, "{color}")
            }
            AST::Argument(argument) => write!(f, "{argument}"),
            AST::Symbol(symbol) => write!(f, "{symbol}"),
        }
    }
}

// An Expr from the parser-pass
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedExpr<'a> {
    pub value: Box<AST<ParsedExpr<'a>>>,
    pub token: Pair<'a, Rule>,
}


pub struct ASTExpr {
    ast: AST<>
}


TypedExpr ( ScopedExpr ( ParsedExpr ( AST ) ) );


impl Expr for ParsedExpr<'_> {
    fn pretty_print(&self) -> StringTreeNode {
        match &*self.value {
            AST::Int(_) => StringTreeNode::new("int".to_string()),
            AST::Float(_) => StringTreeNode::new("float".to_string()),
            AST::String(_) => StringTreeNode::new("string".to_string()),
            AST::Color(_color) => StringTreeNode::new("color".to_string()),
            AST::Symbol(_id) => StringTreeNode::new("id".to_string()),
            AST::FuncCall(func_call) => func_call.pretty_print(),
            AST::Argument(argument) => argument.pretty_print(),
            AST::Array(items) => StringTreeNode::with_child_nodes(
                "array".to_string(),
                items.into_iter().map(|i| i.pretty_print()),
            ),
            AST::Dict(items) => StringTreeNode::with_child_nodes(
                "dict".to_string(),
                items.into_iter().map(|i| {
                    StringTreeNode::with_child_nodes(
                        "entry".to_string(),
                        vec![i.0.pretty_print(), i.1.pretty_print()].into_iter(),
                    )
                }),
            ),
        }
    }
}

impl<'a> ParsedExpr<'a> {
    pub fn new(value: AST<ParsedExpr<'a>>, token: Pair<'a, Rule>) -> Self {
        Self {
            value: Box::new(value),
            token,
        }
    }
}

impl Display for ParsedExpr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub fn filter_comments(pair: &Pair<'_, Rule>) -> bool {
    match pair.as_rule() {
        Rule::EOI | Rule::WHITESPACE | Rule::COMMENT | Rule::line_comment | Rule::program => false,
        _ => true,
    }
}

pub fn parsed_expr_pass<'a>(pair: Pair<'a, Rule>) -> ParsedExpr<'a> {
    let text = pair.as_str();
    let len = text.len();
    match pair.as_rule() {
        Rule::expr => {
            let next = pair
                .clone()
                .into_inner()
                .filter(filter_comments)
                .next()
                .expect("expr inner");
            parsed_expr_pass(next)
        }
        Rule::dict => {
            let mut dict: Vec<(ParsedExpr, ParsedExpr)> = vec![];
            let pairs = pair.clone().into_inner().filter(filter_comments);

            for dict_assign in pairs {
                let mut dict_assign = dict_assign.into_inner().filter(filter_comments);
                let key = dict_assign.next().expect("dict_assign key");
                let key = parsed_expr_pass(key);

                let value = dict_assign.next().expect("dict_assign value");
                let value = parsed_expr_pass(value);

                dict.push((key, value));
            }

            let dict = AST::Dict(dict);
            ParsedExpr::new(dict, pair)
        }
        Rule::array => {
            let mut array: Vec<ParsedExpr> = vec![];
            let values = pair.clone().into_inner().filter(filter_comments);

            for value in values {
                let value = parsed_expr_pass(value);
                array.push(value);
            }

            let array = AST::Array(array);
            ParsedExpr::new(array, pair)
        }
        Rule::func_call => {
            let text = &text[1..len];
            if text.trim() == "" {
                panic!("empty funccall not allowed");
            }

            let mut pairs = pair.clone().into_inner().filter(filter_comments);
            let next = pairs.next().expect(&format!("funccall id {pair}"));

            let func_call = match next.as_rule() {
                Rule::identifier => {
                    let id_expr = parsed_expr_pass(next.clone());
                    let args = pairs
                        .map(parsed_expr_pass)
                        .collect();

                    let f = NonEmptyFuncCall::new(id_expr, args);
                    FuncCall::Single(f)
                }
                Rule::func_call => {
                    let mut nested: Vec<FuncCall<ParsedExpr>> = vec![];

                    let AST::FuncCall(f) = *parsed_expr_pass(next).value else {
                        panic!("funccall* f");
                    };

                    nested.push(f);

                    for pair in pairs {
                        let AST::FuncCall(f) = *parsed_expr_pass(pair).value else {
                            panic!("funccall* f");
                        };

                        nested.push(f);
                    }

                    FuncCall::Multi(nested)
                }
                _ => panic!("illegal funccall"),
            };
            ParsedExpr::new(AST::FuncCall(func_call), pair)
        }
        Rule::argument => {
            let mut pairs = pair.clone().into_inner().filter(filter_comments);
            let first = pairs.next().expect("argument");
            let second = pairs.next();

            let argument = match second {
                Some(second) => {
                    let id = parsed_expr_pass(first);
                    let expr = parsed_expr_pass(second);
                    Argument::Named(NamedArgument::new(id, expr))
                }
                None => {
                    let expr = parsed_expr_pass(first);
                    Argument::Unnamed(expr)
                }
            };

            ParsedExpr::new(AST::Argument(argument), pair)
        }
        Rule::identifier => {
            let id = Symbol::new(text.to_string());
            ParsedExpr::new(AST::Symbol(id), pair)
        }
        Rule::int => {
            let int = text.parse().expect("int");
            ParsedExpr::new(AST::Int(int), pair)
        }
        Rule::float => {
            let float = text[0..len].parse().expect("float");
            ParsedExpr::new(AST::Float(float), pair)
        }
        Rule::string => {
            let s = &text[1..len];
            ParsedExpr::new(AST::String(s.to_string()), pair)
        }
        Rule::color => {
            let r = u8::from_str_radix(&text[1..=2], 16).expect("color");
            let g = u8::from_str_radix(&text[3..=4], 16).expect("color");
            let b = u8::from_str_radix(&text[5..=6], 16).expect("color");
            let color = Color::new(r, g, b);

            ParsedExpr::new(AST::Color(color), pair)
        }
        _ => panic!("invalid pair {pair}"),
    }
}
