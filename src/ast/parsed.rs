use std::fmt::Display;

use text_trees::StringTreeNode;

use miette::Result;

use crate::parser::Rule;

use super::{
    Argument, Array, Color, Dict, DictEntry, Expr, Float, FuncCall, FuncCallList, FuncCallSingle,
    Int, NamedArgument, PrettyPrintable, StageInfo, Str, Symbol,
};

// Helper function to create parse errors
fn parse_error(message: &str, pair: Option<&Pair>) -> miette::Report {
    match pair {
        Some(p) => {
            let span = p.as_span();
            miette::miette!(
                labels = vec![miette::LabeledSpan::at(
                    span.start()..span.end(),
                    "here"
                )],
                "Parse error: {}", message
            )
        }
        None => miette::miette!("Parse error: {}", message)
    }
}

pub type Pair = pest::iterators::Pair<'static, Rule>;

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ParsedStageInfo {
    pub token: Pair,
}

impl ParsedStageInfo {
    pub fn new(token: Pair) -> Self {
        Self { token }
    }
}

impl PrettyPrintable for ParsedStageInfo {
    fn pretty_print(&self) -> StringTreeNode {
        // StringTreeNode::new(format!("parsed_stage_info: {self}"))
        StringTreeNode::new("".to_string())
    }
}

impl Display for ParsedStageInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "{{ token: {} }}", self.token)
        write!(f, "")
    }
}

impl StageInfo for ParsedStageInfo {}

pub fn is_comment(pair: &Pair) -> bool {
    matches!(
        pair.as_rule(),
        Rule::EOI | Rule::WHITESPACE | Rule::COMMENT | Rule::line_comment | Rule::program
    )
}

pub fn is_not_comment(pair: &Pair) -> bool {
    !is_comment(pair)
}

pub fn parsed_expr_pass(pair: Pair) -> Result<Expr<ParsedStageInfo>> {
    let text = pair.as_str();
    let len = text.len();
    match pair.as_rule() {
        Rule::expr => {
            let next = pair
                .clone()
                .into_inner()
                .find(is_not_comment)
                .ok_or_else(|| parse_error("empty expression", Some(&pair)))?;
            parsed_expr_pass(next)
        }
        Rule::dict => {
            let mut entries = vec![];
            let pairs = pair.clone().into_inner().filter(is_not_comment);

            for dict_assign in pairs {
                let mut dict_assign_vals = dict_assign.clone().into_inner().filter(is_not_comment);
                let key = dict_assign_vals.next().ok_or_else(|| parse_error("missing key in dictionary entry", Some(&dict_assign)))?;
                let key = parsed_expr_pass(key)?;

                let value = dict_assign_vals.next().ok_or_else(|| parse_error("missing value in dictionary entry", Some(&dict_assign)))?;
                let value = parsed_expr_pass(value)?;

                let entry = DictEntry::new(key, value, info(dict_assign));
                entries.push(entry);
            }

            Ok(Expr::Dict(Dict::new(entries, info(pair))))
        }
        Rule::array => {
            let mut array = vec![];
            let values = pair.clone().into_inner().filter(is_not_comment);

            for value in values {
                let value = parsed_expr_pass(value)?;
                array.push(value);
            }

            Ok(Expr::Array(Array::new(array, info(pair))))
        }
        Rule::func_call => {
            let text = &text[1..len];
            if text.trim() == "" {
                return Err(parse_error("empty function call not allowed", Some(&pair)));
            }

            let mut pairs = pair.clone().into_inner().filter(is_not_comment);
            let next = pairs.next().ok_or_else(|| {
                parse_error(&format!("function call id missing: {}", pair.as_str()), Some(&pair))
            })?;

            match next.as_rule() {
                // multi-funccall
                Rule::func_call => {
                    let mut nested: Vec<FuncCall<ParsedStageInfo>> = vec![];

                    let f = parsed_expr_pass(next.clone())?;
                    let Expr::FuncCall(f) = f else {
                        return Err(parse_error("expected function call in nested function call", Some(&next)));
                    };

                    nested.push(f);

                    for pair in pairs {
                        let Expr::FuncCall(f) = parsed_expr_pass(pair.clone())? else {
                            return Err(parse_error("expected function call in function call list", Some(&pair)));
                        };

                        nested.push(f);
                    }

                    Ok(Expr::FuncCall(FuncCall::List(FuncCallList::new(nested, info(pair)))))
                }
                Rule::normal_func_call | Rule::def_func_call | Rule::lambda_func_call => {
                    parsed_expr_pass(next)
                }
                _ => return Err(parse_error("illegal function call rule", Some(&next))),
            }
        }
        Rule::def_func_call => {
            let mut pairs = pair.clone().into_inner(); 
            let def = parsed_expr_pass(pairs.next().ok_or_else(|| parse_error("missing def keyword", Some(&pair)))?)?;
            let Expr::Symbol(def) = def else {
                return Err(parse_error("expected symbol for def function call", Some(&pair)));
            };

            let id = parsed_expr_pass(pairs.next().ok_or_else(|| parse_error("missing function identifier", Some(&pair)))?)?;
            if !matches!(id, Expr::Symbol(_)) {
                return Err(parse_error("expected symbol for function identifier", Some(&pair)));
            }

            let next = pairs.next().ok_or_else(|| parse_error("missing function arguments or body", Some(&pair)))?;
            let f = match next.as_rule() {
                Rule::args_list => {
                    let mut args = vec![Argument::Positional(id)];
                    let argslist_args = extract_arguments(next)?;
                    args.extend(argslist_args);
                    let body = parsed_expr_pass(pairs.next().ok_or_else(|| parse_error("missing function body", Some(&pair)))?)?;
                    args.push(Argument::Positional(body));
                    FuncCallSingle::new(def, args, info(pair))
                },
                Rule::expr => {
                    let body = parsed_expr_pass(next)?;
                    let args = vec![Argument::Positional(id), Argument::Positional(body)];
                    FuncCallSingle::new(def, args, info(pair))
                },
                _ => return Err(parse_error("invalid def function call structure", Some(&next)))
            };

            Ok(Expr::FuncCall(FuncCall::Single(f)))
        }
        Rule::lambda_func_call => {
            let mut pairs = pair.clone().into_inner(); 
            let lambda = parsed_expr_pass(pairs.next().ok_or_else(|| parse_error("missing lambda keyword", Some(&pair)))?)?;
            let Expr::Symbol(lambda) = lambda else {
                return Err(parse_error("expected symbol for lambda function call", Some(&pair)));
            };

            let next = pairs.next().ok_or_else(|| parse_error("missing lambda arguments or body", Some(&pair)))?;
            let f = match next.as_rule() {
                Rule::args_list => {
                    let mut args = vec![];
                    let argslist_args = extract_arguments(next)?;
                    args.extend(argslist_args);
                    let body = parsed_expr_pass(pairs.next().ok_or_else(|| parse_error("missing lambda body", Some(&pair)))?)?;
                    args.push(Argument::Positional(body));
                    FuncCallSingle::new(lambda, args, info(pair))
                },
                Rule::expr => {
                    let body = parsed_expr_pass(next)?;
                    let args = vec![Argument::Positional(body)];
                    FuncCallSingle::new(lambda, args, info(pair))
                },
                _ => return Err(parse_error("invalid lambda function call structure", Some(&next)))
            };

            Ok(Expr::FuncCall(FuncCall::Single(f)))
        }
        Rule::normal_func_call => {
            let mut pairs = pair.clone().into_inner();
            let id = parsed_expr_pass(pairs.next().ok_or_else(|| parse_error("missing function identifier", Some(&pair)))?)?;
            let Expr::Symbol(id) = id else {
                return Err(parse_error("expected symbol for normal function call", Some(&pair)));
            };

            if let Some(args_pair) = pairs.next() {
                let args = extract_arguments(args_pair)?;
                Ok(Expr::FuncCall(FuncCall::Single(FuncCallSingle::new(id, args, info(pair)))))
            } else {
                Ok(Expr::FuncCall(FuncCall::Single(FuncCallSingle::new(
                    id,
                    vec![],
                    info(pair),
                ))))
            }
        }
        Rule::named_argument | Rule::positional_argument => {
            return Err(parse_error("arguments should not be parsed directly", Some(&pair)));
        }
        Rule::identifier | Rule::def_id | Rule::lambda_id => Ok(Expr::Symbol(Symbol::new(text.to_string(), info(pair)))),
        Rule::int => {
            let int = text.parse().map_err(|_| parse_error("invalid integer format", Some(&pair)))?;
            Ok(Expr::Int(Int::new(int, info(pair))))
        }
        Rule::float => {
            let float = text[0..len - 1].parse().map_err(|_| parse_error("invalid float format", Some(&pair)))?;
            Ok(Expr::Float(Float::new(float, info(pair))))
        }
        Rule::string => {
            let s = &text[1..len - 1];
            Ok(Expr::String(Str::new(s.to_string(), info(pair))))
        }
        Rule::color => {
            let r = u8::from_str_radix(&text[1..=2], 16).map_err(|_| parse_error("invalid red component in color", Some(&pair)))?;
            let g = u8::from_str_radix(&text[3..=4], 16).map_err(|_| parse_error("invalid green component in color", Some(&pair)))?;
            let b = u8::from_str_radix(&text[5..=6], 16).map_err(|_| parse_error("invalid blue component in color", Some(&pair)))?;
            if text.len() > 7 {
                let alpha = u8::from_str_radix(&text[7..=8], 16).map_err(|_| parse_error("invalid alpha component in color", Some(&pair)))?;
                Ok(Expr::Color(Color::new(r, g, b, alpha, info(pair))))
            } else {
                Ok(Expr::Color(Color::new(r, g, b, 255, info(pair))))
            }
        },
        _ => return Err(parse_error(&format!("unsupported parser rule: {:?}", pair.as_rule()), Some(&pair))),
    }
}

fn info(p: Pair) -> ParsedStageInfo{
    ParsedStageInfo { token: p }
}

fn extract_arguments(p: Pair) -> Result<Vec<Argument<ParsedStageInfo>>> {
    let mut is_args_list = false;
    let pairs = match p.as_rule() {
        Rule::args_list => {
            is_args_list = true;
            p.clone().into_inner().next().ok_or_else(|| {
                parse_error("empty args list", Some(&p))
            })?.into_inner()
        },
        Rule::func_arguments => {
            p.into_inner()
        },
        _ => return Err(parse_error("expected args_list or func_arguments", Some(&p)))
    };

    let mut args = vec![];
    let mut no_more_positional_args = false;

    for pair in pairs {
        let mut inner_pairs = pair.clone().into_inner();
        let arg = match pair.as_rule() {
            Rule::named_argument => {
                no_more_positional_args = true;

                let symbol = parsed_expr_pass(inner_pairs.next().ok_or_else(|| parse_error("missing symbol in named argument", Some(&pair)))?)?;
                let Expr::Symbol(symbol) = symbol else {
                    return Err(parse_error("expected symbol for named argument", Some(&pair)));
                };
                let expr = parsed_expr_pass(inner_pairs.next().ok_or_else(|| parse_error("missing expression in named argument", Some(&pair)))?)?;
                Argument::Named(NamedArgument::new(symbol, expr, info(pair)))
            }
            Rule::positional_argument => {
                if !is_args_list && no_more_positional_args {
                    return Err(parse_error("positional arguments not allowed after named arguments", Some(&pair)));
                }

                let expr = parsed_expr_pass(inner_pairs.next().ok_or_else(|| parse_error("missing expression in positional argument", Some(&pair)))?)?;
                Argument::Positional(expr)
            }
            _ => return Err(parse_error(&format!("illegal argument rule: {:?}", pair.as_rule()), Some(&pair))),
        };
        args.push(arg);
    }

    Ok(args)
}
