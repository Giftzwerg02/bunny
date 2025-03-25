#![allow(dead_code)]

pub mod expressions;
pub mod symbol_table;

use std::fmt::{Debug, Display};

use expressions::{Argument, Color, FuncCall, Id, NamedArgument, NonEmptyFuncCall};
use pest::iterators::Pair;
use symbol_table::SymbolTable;

use crate::parser::Rule;

pub trait Expr: Clone + Debug + PartialEq + Display {}

#[derive(Clone, Debug, PartialEq)]
pub enum AST<TExpr: Expr> {
    Int(u64),
    Float(f64),
    String(String),
    Color(Color),
    Id(Id),
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
            AST::Id(id) => write!(f, "{id}"),
            AST::Argument(argument) => write!(f, "{argument}"),
        }
    }
}

// An Expr from the parser-pass
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedExpr<'a> {
    value: Box<AST<ParsedExpr<'a>>>,
    token: Pair<'a, Rule>,
}

impl Expr for ParsedExpr<'_> {}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedExpr<'a> {
    syms: SymbolTable<NamedExpr<'a>>,
    value: Box<AST<NamedExpr<'a>>>,
    inner: ParsedExpr<'a>,
}

impl Expr for NamedExpr<'_> {}

impl Display for NamedExpr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<'a> ParsedExpr<'a> {
    fn new(value: AST<ParsedExpr<'a>>, token: Pair<'a, Rule>) -> Self {
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

// trait Pass<Src>: From<Src> + Into<Src> {}

fn parsed_expr_pass<'a>(pair: Pair<'a, Rule>, parent: &ParsedExpr<'a>) -> ParsedExpr<'a> {
    let text = pair.as_str();
    let len = text.len();
    match pair.as_rule() {
        Rule::expr => {
            let next = pair.clone().into_inner().next().expect("expr inner");
            parsed_expr_pass(next, parent)
        }
        Rule::dict => {
            let mut dict: Vec<(ParsedExpr, ParsedExpr)> = vec![];
            let pairs = pair.clone().into_inner();

            for dict_assign in pairs {
                let mut dict_assign = dict_assign.into_inner();
                let key = dict_assign.next().expect("dict_assign key");
                let key = parsed_expr_pass(key, parent);

                let value = dict_assign.next().expect("dict_assign value");
                let value = parsed_expr_pass(value, parent);

                dict.push((key, value));
            }

            let dict = AST::Dict(dict);
            ParsedExpr::new(dict, pair)
        }
        Rule::array => {
            let mut array: Vec<ParsedExpr> = vec![];
            let values = pair.clone().into_inner();

            for value in values {
                let value = parsed_expr_pass(value, parent);
                array.push(value);
            }

            let array = AST::Array(array);
            ParsedExpr::new(array, pair)
        }
        Rule::func_call => {
            let text = &text[1..len];
            let func_call = if text.trim() == "" {
                FuncCall::Empty
            } else {
                let mut pairs = pair.clone().into_inner();
                let id = pairs.next().expect("funccall id");
                let AST::Id(id) = *parsed_expr_pass(id, parent).value else {
                    panic!("funccall id");
                };

                let args = pairs
                    .map(|arg| {
                        let AST::Argument(arg) = *parsed_expr_pass(arg, parent).value else {
                            panic!("funccall arg");
                        };

                        arg
                    })
                    .collect();

                let f = NonEmptyFuncCall::new(id, args);
                FuncCall::Call(f)
            };
            ParsedExpr::new(AST::FuncCall(func_call), pair)
        }
        Rule::argument => {
            let mut pairs = pair.clone().into_inner();
            let first = pairs.next().expect("argument");
            let second = pairs.next();

            let argument = match second {
                Some(second) => {
                    let AST::Id(id) = *parsed_expr_pass(first, parent).value else {
                        panic!("argument name");
                    };

                    let expr = parsed_expr_pass(second, parent);
                    Argument::Named(NamedArgument::new(id, expr))
                }
                None => {
                    let expr = parsed_expr_pass(first, parent);
                    Argument::Unnamed(expr)
                }
            };

            ParsedExpr::new(AST::Argument(argument), pair)
        }
        Rule::identifier => {
            let id = Id::new(text.to_string());
            ParsedExpr::new(AST::Id(id), pair)
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
        _ => panic!("ParsedExpr: invalid pair {pair}"),
    }
}
