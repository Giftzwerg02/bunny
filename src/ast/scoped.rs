use core::panic;
use std::{collections::HashMap, fmt::Display};

use text_trees::StringTreeNode;

use crate::ast::{Argument, FuncCallSingle, PositionalArgument};

use super::{
    Array, Color, Dict, DictEntry, Expr, Float, FuncCall, FuncCallList, Int, NamedArgument,
    PrettyPrintable, StageInfo, Str, Symbol, parsed::ParsedStageInfo,
};

#[derive(Debug, Clone)]
pub struct ScopedStageInfo<'a> {
    inner: ParsedStageInfo<'a>,
    syms: SymbolTable<ScopedStageInfo<'a>>,
}

impl<'a> ScopedStageInfo<'a> {
    pub fn new(inner: ParsedStageInfo<'a>, syms: SymbolTable<ScopedStageInfo<'a>>) -> Self {
        Self { inner, syms }
    }
}

impl<'a> PrettyPrintable for ScopedStageInfo<'a> {
    fn pretty_print(&self) -> StringTreeNode {
        StringTreeNode::new(format!(""))
    }
}

impl<'a> Display for ScopedStageInfo<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ syms: {}, inner: {} }}", self.syms, self.inner)
    }
}

impl<'a> StageInfo for ScopedStageInfo<'a> {}

#[derive(Clone, Debug)]
pub struct SymbolTable<I: StageInfo> {
    inner: HashMap<String, Expr<I>>,
}

impl<I: StageInfo> Display for SymbolTable<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.inner.keys())
    }
}

impl<I: StageInfo> SymbolTable<I> {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn insert(&mut self, sym: String, value: Expr<I>) -> bool {
        self.inner.insert(sym, value).is_some()
    }

    pub fn with(&self, sym: String, value: Expr<I>) -> Self {
        let mut cpy = self.clone();
        cpy.insert(sym, value);
        cpy
    }

    pub fn contains(&self, sym: &str) -> bool {
        self.inner.contains_key(sym)
    }

    pub fn get(&self, sym: &str) -> Option<&Expr<I>> {
        self.inner.get(sym)
    }
}

pub fn scoped_expr_pass<'a>(
    src: Expr<ParsedStageInfo<'a>>,
    syms: &SymbolTable<ScopedStageInfo<'a>>,
) -> Expr<ScopedStageInfo<'a>> {
    match src {
        Expr::Int(int) => Expr::Int(Int::new(int.value, info(int.info, syms.clone()))),
        Expr::Float(float) => Expr::Float(Float::new(float.value, info(float.info, syms.clone()))),
        Expr::String(str) => Expr::String(Str::new(str.value, info(str.info, syms.clone()))),
        Expr::Color(color) => Expr::Color(Color::new(
            color.r,
            color.g,
            color.b,
            info(color.info, syms.clone()),
        )),
        Expr::Array(array) => {
            let info = info(array.info, syms.clone());
            let array = array
                .value
                .into_iter()
                .map(|v| scoped_expr_pass(v, syms))
                .collect();

            Expr::Array(Array::new(array, info))
        }
        Expr::Dict(dict) => {
            let dict_info = info(dict.info, syms.clone());
            let dict = dict
                .value
                .into_iter()
                .map(|e| {
                    let k = scoped_expr_pass(e.key, syms);
                    let v = scoped_expr_pass(e.value, syms);
                    let dict_entry_info = info(e.info, syms.clone());
                    DictEntry::new(k, v, dict_entry_info)
                })
                .collect();

            Expr::Dict(Dict::new(dict, dict_info))
        }
        Expr::FuncCall(func_call) => {
            match func_call {
                FuncCall::Single(func_call_single) => {
                    // 1. check if function exists
                    if !syms.contains(&func_call_single.id.value) {
                        panic!("undefined: {:?}", func_call_single.id);
                    }

                    // 2. check if it is the def-function (special handling)
                    // TODO: maybe do this a bit cleaner?
                    // (def <name> (<arg1> <arg2> <arg3>) (<body>))
                    // (def foo (x y z) (
                    //      + x y z
                    // ))
                    //
                    // syms.insert(foo, FuncCall(id: +, args: [x, y, z]))
                    //
                    //  ^ id  ^ arg       ^ arg (optional)   ^ arg
                    //  i.e., 2 or 3 args
                    let mut new_syms = syms.clone();
                    if &func_call_single.id.value == "def" {
                        // create a new sym-table entry with the newly defined value
                        let Argument::Positional(ref new_id) = func_call_single.args[0] else {
                            panic!("invalid ast");
                        };

                        let Expr::Symbol(new_id) = *new_id.clone().value else {
                            panic!("invalid ast");
                        };

                        let mut inner_syms = syms.clone();
                        let func_id = Symbol::new(new_id.value, info(new_id.info, syms.clone()));
                        inner_syms.insert(func_id.clone().value, Expr::Symbol(func_id.clone()));

                        if func_call_single.args.len() == 2 {
                            let Argument::Positional(ref new_call) = func_call_single.args[1]
                            else {
                                panic!("invalid ast");
                            };

                            let new_call = scoped_expr_pass(*new_call.clone().value, &inner_syms);

                            new_syms.insert(func_id.value, new_call);

                            dbg!("converting def constant");
                            let mapped_args = func_call_single
                                .args
                                .into_iter()
                                .map(|arg| scoped_expr_pass(Expr::Argument(arg), &inner_syms))
                                .map(|arg| {
                                    let Expr::Argument(arg) = arg else {
                                        panic!("invalid ast");
                                    };
                                    arg
                                })
                                .collect::<Vec<_>>();

                            let ret = FuncCall::Single(FuncCallSingle::new(
                                Symbol::new(
                                    func_call_single.id.value,
                                    info(func_call_single.id.info, new_syms.clone()),
                                ),
                                mapped_args,
                                info(func_call_single.info, new_syms.clone()),
                            ));

                            Expr::FuncCall(ret)
                        } else if func_call_single.args.len() == 3 {
                            let func_args = func_call_single.args[1].clone();
                            let Argument::Positional(func_args) = func_args else {
                                panic!("invalid ast");
                            };

                            let Expr::FuncCall(FuncCall::Single(func_args)) = *func_args.value
                            else {
                                panic!("invalid ast");
                            };

                            inner_syms.insert(
                                func_args.id.value.clone(),
                                argument_symbol(func_args.id, &inner_syms),
                            );

                            for next_arg in func_args.args {
                                let Argument::Positional(next_arg) = next_arg else {
                                    panic!("invalid ast");
                                };

                                let Expr::Symbol(next_arg) = *next_arg.value else {
                                    panic!("invalid ast");
                                };

                                inner_syms.insert(
                                    next_arg.value.clone(),
                                    argument_symbol(next_arg, &inner_syms),
                                );
                            }

                            let Argument::Positional(ref new_call) = func_call_single.args[2]
                            else {
                                panic!("invalid ast");
                            };

                            let new_call = scoped_expr_pass(*new_call.value.clone(), &inner_syms);

                            new_syms.insert(func_id.value, new_call);

                            dbg!("converting def function");
                            let mapped_args = func_call_single
                                .args
                                .into_iter()
                                .map(|arg| scoped_expr_pass(Expr::Argument(arg), &inner_syms))
                                .map(|arg| {
                                    let Expr::Argument(arg) = arg else {
                                        panic!("invalid ast");
                                    };
                                    arg
                                })
                                .collect::<Vec<_>>();

                            let ret = FuncCall::Single(FuncCallSingle::new(
                                Symbol::new(
                                    func_call_single.id.value,
                                    info(func_call_single.id.info, syms.clone()),
                                ),
                                mapped_args,
                                info(func_call_single.info, new_syms.clone()),
                            ));

                            Expr::FuncCall(ret)
                        } else {
                            panic!("invalid ast");
                        }
                    } else {
                        // 3. it is not a def function -> check the inserted symbols
                        // let Expr::FuncCall(FuncCall::Single(func_declaration)) =
                        //     syms.get(&func_call_single.id.value).expect("should exist")
                        // else {
                        //     panic!("invalid ast");
                        // };
                        //
                        // // 3.1 check named arguments
                        // for arg in func_call_single.args {
                        //     if let Argument::Named(arg) = arg {
                        //     }
                        // }

                        dbg!("converting normal function call");
                        let mapped_args = func_call_single
                            .args
                            .into_iter()
                            .map(|arg| scoped_expr_pass(Expr::Argument(arg), &syms))
                            .map(|arg| {
                                let Expr::Argument(arg) = arg else {
                                    panic!("invalid ast");
                                };
                                arg
                            })
                            .collect::<Vec<_>>();

                        let ret = FuncCall::Single(FuncCallSingle::new(
                            Symbol::new(
                                func_call_single.id.value,
                                info(func_call_single.id.info, syms.clone()),
                            ),
                            mapped_args,
                            info(func_call_single.info, syms.clone()),
                        ));

                        Expr::FuncCall(ret)
                    }
                }
                FuncCall::List(func_call_list) => {
                    let mut funcs = vec![];
                    let mut new_syms = syms.clone();
                    for call in func_call_list.calls {
                        let Expr::FuncCall(passed_call) =
                            scoped_expr_pass(Expr::FuncCall(call), &new_syms)
                        else {
                            panic!("invalid ast");
                        };

                        match passed_call {
                            FuncCall::Single(ref f) => {
                                new_syms = f.info.syms.clone();
                            }
                            FuncCall::List(ref f) => {
                                new_syms = f.info.syms.clone();
                            }
                        }

                        funcs.push(passed_call);
                    }

                    Expr::FuncCall(FuncCall::List(FuncCallList::new(
                        funcs,
                        info(func_call_list.info, syms.clone()),
                    )))
                }
            }
        }
        Expr::Argument(argument) => {
            let arg = match argument {
                Argument::Positional(positional_argument) => {
                    let passed = scoped_expr_pass(*positional_argument.value, syms);
                    Argument::Positional(PositionalArgument::new(
                        passed,
                        info(positional_argument.info, syms.clone()),
                    ))
                }
                Argument::Named(named_argument) => {
                    let Expr::Symbol(name) =
                        scoped_expr_pass(Expr::Symbol(named_argument.name), syms)
                    else {
                        panic!("invalid ast");
                    };
                    let passed = scoped_expr_pass(*named_argument.value, syms);
                    Argument::Named(NamedArgument::new(
                        name,
                        passed,
                        info(named_argument.info, syms.clone()),
                    ))
                }
            };

            Expr::Argument(arg)
        }
        Expr::Symbol(symbol) => {
            Expr::Symbol(Symbol::new(symbol.value, info(symbol.info, syms.clone())))
        }
    }
}

fn info<'a>(
    parsed: ParsedStageInfo<'a>,
    syms: SymbolTable<ScopedStageInfo<'a>>,
) -> ScopedStageInfo<'a> {
    ScopedStageInfo {
        inner: parsed,
        syms,
    }
}

fn argument_symbol<'a>(
    id: Symbol<ParsedStageInfo<'a>>,
    syms: &SymbolTable<ScopedStageInfo<'a>>,
) -> Expr<ScopedStageInfo<'a>> {
    Expr::Argument(Argument::Positional(PositionalArgument::new(
        Expr::Symbol(Symbol::new(id.value, info(id.info.clone(), syms.clone()))),
        info(id.info.clone(), syms.clone()),
    )))
}
