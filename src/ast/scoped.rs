use core::panic;
use std::{any::type_name_of_val, fmt::Display};

use text_trees::StringTreeNode;
use im::HashMap;

use crate::ast::{Argument, FuncCallSingle, PositionalArgument};

use super::{
    Array, Color, Dict, DictEntry, Expr, Float, FuncCall, FuncCallList, Int, NamedArgument,
    PrettyPrintable, StageInfo, Str, Symbol, parsed::ParsedStageInfo,
};

#[derive(Debug, Clone)]
pub struct ScopedStageInfo<'a> {
    pub inner: ParsedStageInfo<'a>,
    pub syms: SymbolTable<ScopedStageInfo<'a>>,
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
    pub inner: HashMap<String, Expr<I>>,
}

impl<I: StageInfo> Display for SymbolTable<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.inner.keys().map(|k| k.to_string()).collect::<Vec<_>>())
    }
}

impl<I: StageInfo> SymbolTable<I> {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn insert(&mut self, sym: String, value: Expr<I>) {
        self.inner.insert(sym, value);
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

const FUNC_DEF_KEYWORD: &str = "def";

pub fn scoped_expr_pass<'a>(
    src: Expr<ParsedStageInfo<'a>>,
    syms: &SymbolTable<ScopedStageInfo<'a>>,
) -> Expr<ScopedStageInfo<'a>> {
    match src {
        Expr::Int(int) => Expr::Int(Int::new(int.value, info(int.info, syms.clone()))),
        Expr::Float(float) => Expr::Float(Float::new(float.value, info(float.info, syms.clone()))),
        Expr::String(str) => Expr::String(Str::new(str.value, info(str.info, syms.clone()))),
        Expr::Color(color) => Expr::Color(Color::new(color.r, color.g, color.b, info(color.info, syms.clone()))),
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
                    if &func_call_single.id.value == FUNC_DEF_KEYWORD {
                        // create a new sym-table entry with the newly defined value
                        let Argument::Positional(ref new_id) = func_call_single.args[0] else {
                            panic!("invalid ast");
                        };

                        let Expr::Symbol(new_id) = *new_id.clone().value else {
                            panic!("invalid ast");
                        };

                        let mut inner_syms = syms.clone();
                        let func_id = pass_symbol(new_id.clone(), inner_syms.clone());
                        inner_syms.insert(func_id.clone().value, Expr::Symbol(func_id.clone()));

                        if func_call_single.args.len() == 2 {
                            let Argument::Positional(ref new_call) = func_call_single.args[1]
                            else {
                                panic!("invalid ast");
                            };

                            let call_pos = new_call.info.clone();
                            let new_call = scoped_expr_pass(*new_call.clone().value, &inner_syms);

                            // We just created a "constant" function, i.e., with no arguments
                            // Therefore, the created function where the func_id should reference
                            // (in the symbol table) a function with a singular argument, that
                            // being the body of the function.
                            let f_to_insert = FuncCallSingle::new(
                                func_id.clone(),
                                vec![Argument::Positional(PositionalArgument::new(
                                    new_call.clone(),
                                    einfo(call_pos),
                                ))],
                                func_id.info,
                            );

                            let f_to_insert = Expr::FuncCall(FuncCall::Single(f_to_insert));

                            new_syms.insert(func_id.value, f_to_insert);

                            let mapped_arg0 = argument_symbol(new_id, &inner_syms);
                            let mapped_arg1 =
                                Expr::Argument(Argument::Positional(PositionalArgument::new(
                                    new_call.clone(),
                                    einfo(func_call_single.args[1].info().clone()),
                                )));
                            let mapped_args = vec![mapped_arg0, mapped_arg1]
                                .into_iter()
                                .map(|arg| {
                                    let Expr::Argument(arg) = arg else {
                                        panic!("invalid ast");
                                    };
                                    arg
                                })
                                .collect();

                            let ret = FuncCall::Single(FuncCallSingle::new(
                                pass_symbol(func_call_single.id, new_syms.clone()),
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

                            // We just created a parametric function, i.e., with n >= 1 arguments.
                            // Therefore, the created function where the func_id should reference
                            // (in the symbol table) a function with n+1 arguments, that being the
                            // n arguments + the body of the function.
                            inner_syms.insert(
                                func_args.id.value.clone(),
                                argument_symbol(func_args.id.clone(), &inner_syms),
                            );

                            for next_arg in &func_args.args {
                                let Argument::Positional(next_arg) = next_arg else {
                                    panic!("invalid ast");
                                };

                                let Expr::Symbol(ref next_arg) = *next_arg.value else {
                                    panic!("invalid ast");
                                };

                                inner_syms.insert(
                                    next_arg.value.clone(),
                                    argument_symbol(next_arg.clone(), &inner_syms),
                                );
                            }

                            let Argument::Positional(ref new_call) = func_call_single.args[2]
                            else {
                                panic!("invalid ast");
                            };

                            let call_pos = new_call.info.clone();
                            let new_call = scoped_expr_pass(*new_call.value.clone(), &inner_syms);
                            let new_call_args = arguments_list(func_args.clone())
                                .into_iter()
                                .map(|arg| {
                                    let Expr::Argument(arg) =
                                        scoped_expr_pass(Expr::Argument(arg), &inner_syms)
                                    else {
                                        panic!("invalid ast");
                                    };
                                    arg
                                })
                                .collect();

                            let f_to_insert = FuncCallSingle::new(
                                func_id.clone(),
                                vec![
                                    new_call_args,
                                    vec![Argument::Positional(PositionalArgument::new(
                                        new_call.clone(),
                                        einfo(call_pos),
                                    ))],
                                ]
                                .concat(),
                                func_id.info,
                            );

                            let f_to_insert = Expr::FuncCall(FuncCall::Single(f_to_insert));

                            new_syms.insert(func_id.value, f_to_insert);

                            let mapped_arg0 = argument_symbol(new_id, &inner_syms);
                            let mapped_arg1 = arguments_symbol_list(func_args, inner_syms);
                            let mapped_arg2 =
                                Expr::Argument(Argument::Positional(PositionalArgument::new(
                                    new_call.clone(),
                                    einfo(func_call_single.args[2].info().clone()),
                                )));
                            let mapped_args = vec![mapped_arg0, mapped_arg1, mapped_arg2]
                                .into_iter()
                                .map(|arg| {
                                    let Expr::Argument(arg) = arg else {
                                        panic!("invalid ast");
                                    };
                                    arg
                                })
                                .collect();

                            let ret = FuncCall::Single(FuncCallSingle::new(
                                pass_symbol(func_call_single.id, new_syms.clone()),
                                mapped_args,
                                info(func_call_single.info, new_syms.clone()),
                            ));

                            Expr::FuncCall(ret)
                        } else {
                            panic!("invalid ast");
                        }
                    } else {
                        // 3. it is not a def function -> check the inserted symbols
                        let Some(func_declaration) = syms.get(&func_call_single.id.value) else {
                            panic!("not defined: {}: {}", &func_call_single.id.value, syms);
                        };

                        let Expr::FuncCall(FuncCall::Single(func_declaration)) = func_declaration
                        else {
                            panic!(
                                "({}): symbol is not a function: {}: {}",
                                func_call_single.id.value,
                                type_name_of_val(&func_declaration),
                                func_declaration.name()
                            );
                        };

                        let decl_args = params(func_declaration);

                        let mut used = vec![];
                        for arg in &func_call_single.args {
                            if let Argument::Named(arg) = arg {
                                let name = &arg.name;

                                let mut found = false;
                                for decl_arg in decl_args {
                                    let Argument::Positional(decl_arg) = decl_arg else {
                                        panic!("invalid ast");
                                    };
                                    let Expr::Symbol(ref decl_arg) = *decl_arg.value else {
                                        panic!("invalid ast:\n{}", decl_arg.value.pretty_print());
                                    };
                                    if decl_arg.value == name.value {
                                        found = true;
                                        if used.contains(&name.value) {
                                            panic!("used named argument twice");
                                        }
                                        used.push(name.value.clone());
                                    }
                                }

                                if !found {
                                    panic!("named argument references unknown parameter: {}", name);
                                }
                            }
                        }

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
                            pass_symbol(func_call_single.id, new_syms.clone()),
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
                        einfo(positional_argument.info),
                    ))
                }
                Argument::Named(named_argument) => {
                    let name = pass_symbol(named_argument.name, syms.clone());
                    let passed = scoped_expr_pass(*named_argument.value, syms);
                    Argument::Named(NamedArgument::new(name, passed, einfo(named_argument.info)))
                }
            };

            Expr::Argument(arg)
        }
        Expr::Symbol(symbol) => {
            let s = Symbol::new(symbol.value.clone(), info(symbol.info, syms.clone()));
            if !syms.contains(&symbol.value) {
                panic!("undefined symbol: {}", symbol.value)
            }
            Expr::Symbol(s)
        }
    }
}

fn pass_symbol<'a>(symbol: Symbol<ParsedStageInfo<'a>>, table: SymbolTable<ScopedStageInfo<'a>>) -> Symbol<ScopedStageInfo<'a>> {
    Symbol::new(symbol.value.clone(), info(symbol.info.clone(), table))
}

// empty info, i.e., empty symbol table
fn einfo<'a>(parsed: ParsedStageInfo<'a>) -> ScopedStageInfo<'a> {
    ScopedStageInfo {
        inner: parsed,
        syms: SymbolTable::new(),
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
        Expr::Symbol(pass_symbol(id.clone(), syms.clone())),
        info(id.info, syms.clone()),
    )))
}

fn arguments_symbol_list<'a>(
    args: FuncCallSingle<ParsedStageInfo<'a>>,
    table: SymbolTable<ScopedStageInfo<'a>>,
) -> Expr<ScopedStageInfo<'a>> {
    Expr::Argument(Argument::Positional(PositionalArgument::new(
        Expr::FuncCall(FuncCall::Single(FuncCallSingle::new(
            pass_symbol(args.id, table.clone()),
            args.args
                .into_iter()
                .map(|arg| {
                    let Argument::Positional(arg) = arg else {
                        panic!("invalid ast");
                    };

                    let Expr::Symbol(arg) = *arg.value else {
                        panic!("invalid ast");
                    };
                    let inf = arg.info.clone();
                    let arg = Expr::Symbol(pass_symbol(arg, table.clone()));
                    Argument::Positional(PositionalArgument::new(arg, info(inf, table.clone())))
                })
                .collect(),
            einfo(args.info.clone()),
        ))),
        einfo(args.info),
    )))
}

fn arguments_list<'a, I: StageInfo>(args: FuncCallSingle<I>) -> Vec<Argument<I>> {
    let a0 = Argument::Positional(PositionalArgument::new(
        Expr::Symbol(args.id.clone()),
        args.id.info,
    ));
    let a_n = args.args;
    let mut res = vec![a0];
    for a in a_n {
        res.push(a);
    }
    res
}

// the last argument of a function declaration is the function body
// this function only returns actual parameters, i.e., if a function has n arguments, it returns
// n-1 parameters
fn params<'a, 'b>(
    decl_func: &'b FuncCallSingle<ScopedStageInfo<'a>>,
) -> &'b [Argument<ScopedStageInfo<'a>>] {
    let args = &decl_func.args;
    if args.len() < 2 {
        return &[];
    }

    &args[0..args.len() - 1]
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{self, parsed::parsed_expr_pass},
        parser::{BunnyParser, Rule},
    };

    use super::*;
    use pest::Parser;

    fn empty_func<I: StageInfo>(info: I) -> ast::FuncCallSingle<I> {
        ast::FuncCallSingle::new(
            ast::Symbol::new("".to_owned(), info.clone()),
            vec![],
            info.clone(),
        )
    }

    fn empty_func_expr<I: StageInfo>(info: I) -> Expr<I> {
        Expr::FuncCall(ast::FuncCall::Single(empty_func(info)))
    }

    fn scoped_test(code: &str) {
        let pair = BunnyParser::parse(Rule::program, &code)
            .unwrap()
            .next()
            .unwrap();

        let mut syms = SymbolTable::new();
        let info = ScopedStageInfo::new(ParsedStageInfo::new(pair.clone()), syms.clone());

        syms.insert("def".to_string(), empty_func_expr(info.clone()));
        syms.insert("+".to_string(), empty_func_expr(info.clone()));

        let parsed_expr = parsed_expr_pass(pair);
        scoped_expr_pass(parsed_expr, &syms);
    }

    fn scoped_panic_test(code: &str) {
        let pair = BunnyParser::parse(Rule::program, &code)
            .unwrap()
            .next()
            .unwrap();

        let mut syms = SymbolTable::new();
        let info = ScopedStageInfo::new(ParsedStageInfo::new(pair.clone()), syms.clone());

        syms.insert("def".to_string(), empty_func_expr(info.clone()));
        syms.insert("+".to_string(), empty_func_expr(info.clone()));

        let parsed_expr = parsed_expr_pass(pair);
        let result = std::panic::catch_unwind(|| scoped_expr_pass(parsed_expr, &syms));
        assert!(result.is_err(), "expected code: {}\nto panic", code)
    }

    #[test]
    fn defined_variables_can_be_referenced_later() {
        scoped_test(
            "
            (
                (def a 5)
                (a)
            )
        ",
        );
    }

    #[test]
    fn defined_variables_cannot_be_referenced_earlier() {
        scoped_panic_test(
            "
            (
                (a)
                (def a 5)
            )
        ",
        );
    }

    #[test]
    fn undefined_variables_cannot_be_referenced() {
        scoped_panic_test(
            "
            (
                (def b 5)
                (a)
            )
        ",
        );
    }

    #[test]
    fn scoped_variables_cannot_be_referenced_out_of_scope() {
        scoped_panic_test(
            "(
            (def a (
                (def b 5)
            ))
            (b)
        )",
        );
    }

    #[test]
    fn arguments_can_be_accessed_within_a_funccall() {
        scoped_test(
            "(
            (def a (x y z) (
                (+ x y z)
            ))
        )",
        );
    }

    #[test]
    fn arguments_cannot_be_accessed_outside_a_funccall() {
        scoped_panic_test(
            "(
            (def a (x y z) (
                (+ x y z)
            ))
            (x)
        )",
        );

        scoped_panic_test(
            "(
            (def a (x y z) (
                (+ x y z)
            ))
            (y)
        )",
        );

        scoped_panic_test(
            "(
            (def a (x y z) (
                (+ x y z)
            ))
            (z)
        )",
        );
    }

    #[test]
    fn arguments_can_be_accessed_inside_a_nested_scope() {
        scoped_test(
            "(
            (def a (x y z) (
                (def b (foo bar baz) (
                    (+ x y z foo bar baz)
                ))
                (b 4 5 6)
            ))
            (a 1 2 3)
        )",
        );
    }

    #[test]
    fn variables_can_be_accessed_inside_a_nested_scope() {
        scoped_test(
            "(
            (def foo (bar baz) (
                (+ bar baz)
            ))
            (def a (x y z) (
                (+ x y z (foo 3 4))
            ))
            (a 1 2 3)
        )",
        );
    }

    #[test]
    fn arguments_can_be_set_by_their_name() {
        scoped_test(
            "(
            (def a (foo bar baz) (
                + foo bar baz
            ))
            (a foo: 1 bar: 2 baz: 3)
        )",
        );
    }

    #[test]
    fn named_arguments_cannot_reference_not_defined_args() {
        scoped_panic_test(
            "(
            (def a (foo bar baz) (
                + foo bar baz
            ))
            (a foo: 1 bar: 2 does-not-exist: 3)
        )",
        );
    }

    #[test]
    fn arguments_cannot_be_referenced_more_than_once() {
        scoped_panic_test(
            "(
            (def a (some-arg) (
                + 1 some-arg
            ))
            (a some-arg: 4 some-arg: 2)
        )",
        );
    }

    #[test]
    fn defined_variables_can_be_used_as_return_values() {
        scoped_test(
            "(
            (def a 5)
            (def b a)
            (b)
        )",
        );
    }

    #[test]
    fn undefined_variables_cannot_be_used_as_return_values() {
        scoped_panic_test(
            "(
            (def b a)
            (b)
        )",
        );
    }

    #[test]
    fn variables_cannot_be_accessed_from_a_dynamic_successor() {
        scoped_panic_test("
            (
                (def a (b) (
                    (def inner 5) 
                    (+ 1 b inner)
                ))
                (def foo (bar) (
                    (+ 2 bar inner)
                ))
                (a (foo 3)) 
            )
        ");

        scoped_panic_test("
            (
                (def a (b) (
                    (def inner 5) 
                    (+ 1 b inner)
                ))
                (def foo (bar) (
                    (+ 2 bar b)
                ))
                (a (foo 3)) 
            )
        ");

        scoped_panic_test("
            (
                (def foo (bar) (
                    (def inner 5)
                    (+ 1 2 inner bar)
                ))
                (def a (b) (
                    (def res (foo 1)) 
                    (+ inner res b)
                ))
                (a 4) 
            )
        ");

        scoped_panic_test("
            (
                (def foo (bar) (
                    (def inner 5)
                    (+ 1 2 inner bar)
                ))
                (def a (b) (
                    (def res (foo 1)) 
                    (+ bar res b)
                ))
                (a 4) 
            )
        ");
    }
}
