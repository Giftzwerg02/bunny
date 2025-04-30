use std::fmt::Display;

use pest::iterators::Pair;
use text_trees::StringTreeNode;

use crate::parser::Rule;

use super::{
    Argument, Array, Color, Dict, DictEntry, Expr, Float, FuncCall, FuncCallList, FuncCallSingle,
    Int, NamedArgument, PrettyPrintable, StageInfo, Str, Symbol,
};

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ParsedStageInfo<'a> {
    token: Pair<'a, Rule>,
}

impl<'a> ParsedStageInfo<'a> {
    pub fn new(token: Pair<'a, Rule>) -> Self {
        Self { token }
    }
}

impl PrettyPrintable for ParsedStageInfo<'_> {
    fn pretty_print(&self) -> StringTreeNode {
        // StringTreeNode::new(format!("parsed_stage_info: {self}"))
        StringTreeNode::new("".to_string())
    }
}

impl Display for ParsedStageInfo<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "{{ token: {} }}", self.token)
        write!(f, "")
    }
}

impl StageInfo for ParsedStageInfo<'_> {}

pub fn is_comment(pair: &Pair<'_, Rule>) -> bool {
    matches!(
        pair.as_rule(),
        Rule::EOI | Rule::WHITESPACE | Rule::COMMENT | Rule::line_comment | Rule::program
    )
}

pub fn is_not_comment(pair: &Pair<'_, Rule>) -> bool {
    !is_comment(pair)
}

pub fn parsed_expr_pass<'a>(pair: Pair<'a, Rule>) -> Expr<ParsedStageInfo<'a>> {
    let text = pair.as_str();
    let len = text.len();
    match pair.as_rule() {
        Rule::expr => {
            let next = pair
                .clone()
                .into_inner()
                .find(is_not_comment)
                .expect("expr inner");
            parsed_expr_pass(next)
        }
        Rule::dict => {
            let mut entries = vec![];
            let pairs = pair.clone().into_inner().filter(is_not_comment);

            for dict_assign in pairs {
                let mut dict_assign_vals = dict_assign.clone().into_inner().filter(is_not_comment);
                let key = dict_assign_vals.next().expect("dict_assign key");
                let key = parsed_expr_pass(key);

                let value = dict_assign_vals.next().expect("dict_assign value");
                let value = parsed_expr_pass(value);

                let entry = DictEntry::new(key, value, info(dict_assign));
                entries.push(entry);
            }

            Expr::Dict(Dict::new(entries, info(pair)))
        }
        Rule::array => {
            let mut array = vec![];
            let values = pair.clone().into_inner().filter(is_not_comment);

            for value in values {
                let value = parsed_expr_pass(value);
                array.push(value);
            }

            Expr::Array(Array::new(array, info(pair)))
        }
        Rule::func_call => {
            let text = &text[1..len];
            if text.trim() == "" {
                panic!("empty funccall not allowed");
            }

            let mut pairs = pair.clone().into_inner().filter(is_not_comment);
            let next = pairs.next().unwrap_or_else(|| panic!("funccall id {pair}"));

            match next.as_rule() {
                // multi-funccall
                Rule::func_call => {
                    let mut nested: Vec<FuncCall<ParsedStageInfo<'a>>> = vec![];

                    let f = parsed_expr_pass(next);
                    let Expr::FuncCall(f) = f else {
                        panic!("invalid ast");
                    };

                    nested.push(f);

                    for pair in pairs {
                        let Expr::FuncCall(f) = parsed_expr_pass(pair) else {
                            panic!("invalid ast");
                        };

                        nested.push(f);
                    }

                    Expr::FuncCall(FuncCall::List(FuncCallList::new(nested, info(pair))))
                }
                Rule::normal_func_call | Rule::def_func_call | Rule::lambda_func_call => {
                    parsed_expr_pass(next)
                }
                _ => panic!("illegal funccall"),
            }
        }
        Rule::def_func_call => {
            let mut pairs = pair.clone().into_inner(); 
            let def = parsed_expr_pass(pairs.next().expect("def_func_call::def"));
            let Expr::Symbol(def) = def else {
                panic!("invalid ast");
            };

            let id = parsed_expr_pass(pairs.next().expect("def_func_call::id"));
            if !matches!(id, Expr::Symbol(_)) {
                panic!("invalid ast");
            }

            let next = pairs.next().expect("def_func_call::after-id");
            let f = match next.as_rule() {
                Rule::args_list => {
                    let mut args = vec![Argument::Positional(id)];
                    let argslist_args = extract_arguments(next);
                    args.extend(argslist_args);
                    let body = parsed_expr_pass(pairs.next().expect("def_func_call::body"));
                    args.push(Argument::Positional(body));
                    FuncCallSingle::new(def, args, info(pair))
                },
                Rule::expr => {
                    let body = parsed_expr_pass(next);
                    let args = vec![Argument::Positional(id), Argument::Positional(body)];
                    FuncCallSingle::new(def, args, info(pair))
                },
                _ => panic!("invalid ast")
            };

            Expr::FuncCall(FuncCall::Single(f))
        }
        Rule::lambda_func_call => {
            let mut pairs = pair.clone().into_inner(); 
            let lambda = parsed_expr_pass(pairs.next().expect("def_func_call::def"));
            let Expr::Symbol(lambda) = lambda else {
                panic!("invalid ast");
            };

            let next = pairs.next().expect("def_func_call::after-id");
            let f = match next.as_rule() {
                Rule::args_list => {
                    let mut args = vec![];
                    let argslist_args = extract_arguments(next);
                    args.extend(argslist_args);
                    let body = parsed_expr_pass(pairs.next().expect("def_func_call::body"));
                    args.push(Argument::Positional(body));
                    FuncCallSingle::new(lambda, args, info(pair))
                },
                Rule::expr => {
                    let body = parsed_expr_pass(next);
                    let args = vec![Argument::Positional(body)];
                    FuncCallSingle::new(lambda, args, info(pair))
                },
                _ => panic!("invalid ast")
            };

            Expr::FuncCall(FuncCall::Single(f))
        }
        Rule::normal_func_call => {
            let mut pairs = pair.clone().into_inner();
            let id = parsed_expr_pass(pairs.next().expect("normal_func_call::id"));
            let Expr::Symbol(id) = id else {
                panic!("invalid ast");
            };

            if let Some(args_pair) = pairs.next() {
                let args = extract_arguments(args_pair);
                Expr::FuncCall(FuncCall::Single(FuncCallSingle::new(id, args, info(pair))))
            } else {
                Expr::FuncCall(FuncCall::Single(FuncCallSingle::new(
                    id,
                    vec![],
                    info(pair),
                )))
            }
        }
        Rule::named_argument | Rule::positional_argument => {
            panic!("Rule::arg shouldn't be called");
        }
        Rule::identifier | Rule::def_id | Rule::lambda_id => Expr::Symbol(Symbol::new(text.to_string(), info(pair))),
        Rule::int => {
            let int = text.parse().expect("int");
            Expr::Int(Int::new(int, info(pair)))
        }
        Rule::float => {
            let float = text[0..len].parse().expect("float");
            Expr::Float(Float::new(float, info(pair)))
        }
        Rule::string => {
            let s = &text[1..len - 1];
            Expr::String(Str::new(s.to_string(), info(pair)))
        }
        Rule::color => {
            let r = u8::from_str_radix(&text[1..=2], 16).expect("color");
            let g = u8::from_str_radix(&text[3..=4], 16).expect("color");
            let b = u8::from_str_radix(&text[5..=6], 16).expect("color");
            Expr::Color(Color::new(r, g, b, info(pair)))
        },
        _ => panic!("invalid pair {pair}"),
    }
}

fn info(p: Pair<'_, Rule>) -> ParsedStageInfo<'_> {
    ParsedStageInfo { token: p }
}

fn extract_arguments(p: Pair<'_, Rule>) -> Vec<Argument<ParsedStageInfo<'_>>> {
    println!("extract_arguments: {:?}", p.as_rule());

    let pairs = match p.as_rule() {
        Rule::args_list => {
            p.into_inner().next().unwrap().into_inner()
        },
        Rule::func_arguments => {
            p.into_inner()
        },
        _ => panic!("invalid ast")
    };

    let mut args = vec![];
    let mut no_more_positional_args = false;

    for pair in pairs {
        let mut inner_pairs = pair.clone().into_inner();
        let arg = match pair.as_rule() {
            Rule::named_argument => {
                no_more_positional_args = true;

                let symbol = parsed_expr_pass(inner_pairs.next().expect("symbol"));
                let Expr::Symbol(symbol) = symbol else {
                    panic!("illegal parse-tree");
                };
                let expr = parsed_expr_pass(inner_pairs.next().expect("expr"));
                Argument::Named(NamedArgument::new(symbol, expr, info(pair)))
            }
            Rule::positional_argument => {
                if no_more_positional_args {
                    panic!("positonal arguments not allowed after any named arguments");
                }

                let expr = parsed_expr_pass(inner_pairs.next().expect("expr"));
                Argument::Positional(expr)
            }
            _ => panic!("illegal argument rule: {}", pair),
        };
        args.push(arg);
    }

    args
}
