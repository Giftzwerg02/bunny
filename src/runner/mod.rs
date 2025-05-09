use std::{
    cell::LazyCell,
    collections::HashMap,
    fmt::Display,
    sync::{Arc, Mutex},
};

use im::Vector;
use palette::Srgba;
use value::{Lazy, LazyLambda, Value};

use crate::{
    ast::{Argument, Expr, FuncCall, FuncCallSingle, Lambda, PrettyPrintable, StageInfo, Symbol},
    lazy,
    library::runnable_expression::{InterpreterSymbolTable, RunnableExpr},
    types::typed::{PolyTypedStageInfo, TypedStageInfo, TypedSymbolTable, TypedValue},
};

pub mod value;

#[derive(Debug, Clone)]
struct SymbolStack {
    inner: Vec<Lazy>,
}

impl SymbolStack {
    fn new() -> Self {
        SymbolStack { inner: Vec::new() }
    }

    fn push(&mut self, val: Lazy) {
        self.inner.push(val);
    }

    fn pop(&mut self) -> Option<Lazy> {
        self.inner.pop()
    }

    fn read(&self) -> Option<Lazy> {
        self.inner.last().cloned()
    }
}

#[derive(Debug, Clone)]
pub struct Runner {
    state: HashMap<String, SymbolStack>,
}

impl Runner {
    pub fn new() -> Self {
        Runner {
            state: HashMap::new(),
        }
    }

    // How to run a bunny expression 101:
    //     It is a expression - i.e., some sort of user-written code. We do not know at this point if
    //     and where it calls to a native function.
    //     Every expression is some sort of composition between literals (int, float, string,
    //     color, ...) and symbols.
    //     Literals are just wrapped into a Lazy
    //     Symbols reference some further RunnableExpr
    //     Assume the following code:
    //     ```
    //     (
    //      (def const 5)
    //
    //      (def sum (a b c) (
    //          (+ a b c const)
    //      ))
    //
    //      (sum 1 2 3)
    //     )
    //     ```
    //     The implementation behind
    //      - `sum` is a *RunnableExpr* of variant *Bunny* with value *FuncCall (single)*
    //      - `const` is a *RunnableExpr* of variant *Bunny* with value *Int*
    //      - `+` is a *RunnableExpr* of variant *Native* (with some func-pointer)
    //
    //      When running the expression `(sum 1 2 3)` with its value being f = FuncCallSingle:
    //          1. get the implementing expression via the symbol-table: `smys.get(f.id)`
    //          2. push states: state["a"] = lazy(1), state["b"] = lazy(2) and state["c"] = lazy(3)
    //          3. run f
    //          4. pop states
    //
    //      Therefore, when we get to a funccall branch, we need to check if the identifier
    //      references user-written code or a native function.
    //      Note that a native function technically doesn't need any scoping-information anymore
    //      since it will just be provided with the arguments that were passed to it.
    pub fn run(
        &mut self,
        expr: Expr<PolyTypedStageInfo<'static>>,
        syms: InterpreterSymbolTable,
    ) -> Lazy {
        match expr {
            Expr::Int(int) => {
                lazy!(Lazy::Int, { int.value.try_into().expect("uint too large") })
            }
            Expr::Float(float) => {
                lazy!(Lazy::Float, float.value)
            }
            Expr::String(string) => {
                lazy!(Lazy::String, string.value.into())
            }
            Expr::Color(color) => {
                lazy!(Lazy::Color, {
                    // TODO: add alpha to color...?
                    let color = Srgba::new(color.r, color.g, color.b, 1);
                    color.into()
                })
            }
            Expr::Array(array) => {
                let mut runner = self.clone();
                lazy!(Lazy::Array, {
                    let mut res = Vector::new();

                    for elem in array.value {
                        let elem = runner.run(elem, syms.clone());
                        res.push_back(elem.into());
                    }

                    res
                })
            }
            Expr::Dict(dict) => {
                let mut runner = self.clone();
                lazy!(Lazy::Dict, {
                    let mut res: im::HashMap<Value, Lazy> = im::HashMap::new();
                    for entry in dict.value {
                        let key = entry.key;
                        let key = runner.run(key, syms.clone());
                        let key = key.eval();

                        let value = entry.value;
                        let value = runner.run(value, syms.clone());

                        res.insert(key, value.into());
                    }
                    res
                })
            }
            Expr::Symbol(symbol) => {
                // if the implementation of a function is just a symbol,
                // then we just have another indirection
                //
                // We are inside some func-call, meaning a variable could either come from:
                // 1. An immediately preceeding "let-binding" (def foo ...)
                // 2. From the outer scope which is stored on the SymbolStack
                //
                // Therefore, if the symbol resolves to an argument we need to check the
                // SymbolStack

                let scope = symbol.info.syms.get(&symbol.value).expect("scoping err");
                match scope {
                    TypedValue::FromLibrary(_) => {
                        lazy!(Lazy::Lambda, {
                            let native = syms.get(&symbol.value).unwrap().clone();
                            let lambda =
                                LazyLambda::new(Arc::new(Mutex::new(move |args: Vector<_>| {
                                    (*native)(args.into_iter().collect())
                                })));
                            lambda
                        })
                    }
                    TypedValue::FromBunny(scope) => {
                        if matches!(scope, Expr::Lambda(_)) {
                            self.run(scope.clone(), syms.clone())
                        } else {
                            self.read_var(symbol.value)
                        }
                    }
                }
            }
            Expr::FuncCall(func) => {
                let FuncCall::Single(func) = func else {
                    panic!("invalid ast");
                };

                let implementation = func.info.syms.get(&func.id.value).expect("scoping err");
                dbg!(&func.id.value);

                let TypedValue::FromBunny(implementation) = implementation else {
                    let args = func
                        .args
                        .into_iter()
                        .map(|arg| match arg {
                            crate::ast::Argument::Positional(arg_expr) => {
                                let syms = syms.clone();
                                let mut runner = self.clone();
                                runner.run(arg_expr, syms)
                            }
                            crate::ast::Argument::Named(named_argument) => {
                                let syms = syms.clone();
                                let mut runner = self.clone();
                                runner.run(*named_argument.value, syms)
                            }
                        })
                        .collect::<Vec<Lazy>>();

                    let native = syms.get(&func.id.value).unwrap().clone();
                    return (*native)(args.clone());
                };

                match implementation {
                    // i.e., the argument is used as a function
                    Expr::Symbol(symbol) => {
                        // since the argument is used as a function, the Lazy must be a Lambda
                        let Lazy::Lambda(lambda) = self.read_var(symbol.value.clone()) else {
                            panic!("argument must be a lambda")
                        };

                        let args = func.args.into_iter().map(|arg| {
                            match arg {
                                Argument::Positional(expr) => {
                                    self.run(expr, syms.clone())
                                },
                                Argument::Named(_) => {
                                    panic!("named arguments are not allowed when passing to an argument-lambda")
                                },
                            }
                        }).collect();

                        let lambda_call = (**lambda).clone().func;
                        let mut lambda_call = lambda_call.lock().unwrap();
                        lambda_call(args)
                    }
                    Expr::Lambda(lambda) => {
                        let mut lambda_set_args = lambda.clone();
                        let lambda_pop_args = lambda.clone();
                        let mut already_set_arguments = vec![];
                        for (pos, arg) in func.args.iter().cloned().enumerate() {
                            match arg {
                                crate::ast::Argument::Positional(arg_expr) => {
                                    let arg_sym = lambda.args[pos].into_def_argument_symbol();
                                    already_set_arguments.push(arg_sym.clone());
                                    let arg_value = self.run(arg_expr, syms.clone());
                                    self.push_var(arg_sym.value, arg_value);
                                }
                                crate::ast::Argument::Named(named_argument) => {
                                    already_set_arguments.push(named_argument.name.clone());
                                    let arg_value = self.run(*named_argument.value, syms.clone());
                                    self.push_var(named_argument.name.value, arg_value);
                                }
                            }
                        }

                        // remove already set arguments
                        for already_set_arg in already_set_arguments {
                            pop_arg_by_name(&mut lambda_set_args.args, &already_set_arg);
                        }

                        // apply the default-value for the remaining ones
                        // since this passed the type-checker, we can assume
                        // that earch remaining argument here actually has a default-value
                        for arg in lambda_set_args.args {
                            let name = arg.into_def_argument_symbol();
                            let default_value = arg
                                .into_def_argument_expr()
                                .expect("non-set argument should have a default value");
                            let default_value = self.run(default_value, syms.clone());
                            self.push_var(name.value, default_value);
                        }

                        // TODO: can we get rid of this clone?
                        let body: Box<Expr<PolyTypedStageInfo<'_>>> = lambda.clone().body;
                        let result = self.run(*body, syms);

                        for arg in lambda_pop_args.args {
                            self.pop_var(arg.into_def_argument_symbol().value);
                        }

                        result
                    }
                    _ => panic!("invalid ast"),
                }
            }
            Expr::Lambda(lambda) => {
                // A lambda should return a lazy of
                // a function that takes some arguments and returns a result

                // auto-execute constant definition
                if lambda.args.len() == 0 {
                    return self.run(*lambda.body, syms.clone());
                }

                let lambda_args = Arc::new(
                    lambda
                        .args
                        .iter()
                        .map(|arg| arg.into_def_argument_symbol().value.clone())
                        .collect::<Vec<_>>(),
                );

                let mut lambda_runner = self.clone();

                let body = *lambda.clone().body;

                lazy!(Lazy::Lambda, {
                    let value = LazyLambda::new(Arc::new(Mutex::new(move |args: Vector<_>| {
                        let body = body.clone();
                        let lambda_args = (*lambda_args).clone();
                        for (pos, arg) in args.iter().cloned().enumerate() {
                            let arg_sym = lambda_args[pos].clone();
                            lambda_runner.push_var(arg_sym, arg);
                        }

                        let result = lambda_runner.run(body, syms.clone());

                        for arg in (lambda_args).clone() {
                            lambda_runner.pop_var(arg);
                        }

                        result
                    })));

                    value
                })
            }
        }
    }

    fn push_var(&mut self, sym: String, val: Lazy) {
        match self.state.get_mut(&sym) {
            Some(stack) => stack.push(val.into()),
            None => {
                let mut stack = SymbolStack::new();
                stack.push(val.into());
                self.state.insert(sym, stack);
            }
        }
    }

    fn pop_var(&mut self, sym: String) {
        let stack = self.state.get_mut(&sym).expect("invalid stack");
        stack.pop().expect("invalid stack");
    }

    fn read_var(&mut self, sym: String) -> Lazy {
        let stack = self
            .state
            .get_mut(&sym)
            .expect(&format!("read_var: invalid stack: {sym}"));

        stack
            .read()
            .expect(&format!("read_var: invalid stack: {sym}"))
    }
}

fn pop_arg_by_name<I: StageInfo>(args: &mut Vec<Argument<I>>, name: &Symbol<I>) {
    let idx = args
        .iter()
        .position(|arg| arg.into_def_argument_symbol().value == name.value)
        .expect("called pop_arg_by_name with non-existent symbol-name");
    args.remove(idx);
}

// #[derive(Debug, Clone)]
// struct PolyTypedStageInfo<'a> {
//     // syms: InterpreterSymbolTable<'a, RunnerStageInfo<'a>>,
//     syms: TypedSymbolTable<'a>
// }

// impl<'a> StageInfo for PolyTypedStageInfo<'a> {}
//
// impl<'a> PrettyPrintable for PolyTypedStageInfo<'a> {
//     fn pretty_print(&self) -> text_trees::StringTreeNode {
//         todo!()
//     }
// }
//
// impl<'a> Display for PolyTypedStageInfo<'a> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         todo!()
//     }
// }
