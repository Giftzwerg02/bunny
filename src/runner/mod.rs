use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use palette::Srgba;
use value::{Lazy, LazyFunc, LazyIter, LazyLambda, Value};

use crate::{
    ast::{Expr, FuncCall},
    library::runnable_expression::InterpreterSymbolTable,
    types::typed::{PolyTypedStageInfo, TypedValue},
};

pub mod value;

pub type ToLazyFn = Arc<Mutex<dyn FnMut(Expr<PolyTypedStageInfo<'static>>) -> Lazy>>;
pub type Runnable = Expr<PolyTypedStageInfo<'static>>;

#[derive(Debug, Clone)]
struct SymbolStack {
    inner: Vec<Runnable>,
}

impl SymbolStack {
    fn new() -> Self {
        SymbolStack { inner: Vec::new() }
    }

    fn push(&mut self, val: Runnable) {
        self.inner.push(val);
    }

    fn pop(&mut self) -> Option<Runnable> {
        self.inner.pop()
    }

    fn read(&self) -> Option<Runnable> {
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

    pub fn prepare_syms(mut self, syms: InterpreterSymbolTable) -> ToLazyFn {
        Arc::new(Mutex::new(move |expr| {
            self.run(expr, syms.clone())
        }))
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
        expr: Runnable,
        syms: InterpreterSymbolTable,
    ) -> Lazy {
        match expr {
            Expr::Int(int) => Lazy::new_int(int.value.try_into().expect("uint too large")),
            Expr::Float(float) => Lazy::new_float(float.value),
            Expr::String(string) => Lazy::new_string(string.value.into()),
            Expr::Color(color) => {
                // TODO: add alpha to color...?
                let color = Srgba::new(color.r, color.g, color.b, 1);
                Lazy::new_color(color.into())
            }
            Expr::Array(array) => {
                let iter = array.value
                    .into_iter()
                    .map(|elem| {
                        self.run(elem, syms.clone())
                    });
                let a = LazyIter::new(Arc::new(Mutex::new(Box::new(iter))));
                Lazy::new_array(a)
            }
            Expr::Dict(dict) => {
                let mut res: HashMap<_, _> = HashMap::new();
                for entry in dict.value {
                    let key = entry.key;
                    let key = self.run(key, syms.clone());
                    let key = key.full_eval();

                    let value = LazyFunc::new(Arc::new(Mutex::new(|| {
                        self.run(entry.value, syms.clone())
                    })));

                    res.insert(key, value);
                }
                Lazy::new_dict(res)
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
                        let native = syms.get(&symbol.value).unwrap().clone();
                        let runner = self.clone().prepare_syms(syms);
                        let lambda =
                            LazyLambda::new(Arc::new(Mutex::new(move |args: Vec<_>| {
                                (*native)(args, runner.clone())
                            })));
                        Lazy::new_lambda(lambda)
                    }
                    TypedValue::FromBunny(scope) => {
                        if matches!(scope, Expr::Lambda(_)) {
                            let res = self.run(scope.clone(), syms.clone()).nowrap();
                            res
                        } else {
                            let sym = self.read_var(symbol.value);
                            self.run(sym, syms)
                        }
                    }
                }
            }
            Expr::FuncCall(func) => {
                let FuncCall::Single(func) = func else {
                    panic!("invalid ast");
                };

                let implementation = func.info.syms.get(&func.id.value).expect("scoping err");

                let TypedValue::FromBunny(implementation) = implementation else {
                    let args = func
                        .args
                        .into_iter()
                        .map(|arg| match arg {
                            crate::ast::Argument::Positional(expr) => expr,
                            crate::ast::Argument::Named(named_argument) => *named_argument.value,
                        })
                        .collect::<Vec<_>>();

                    let native = syms.get(&func.id.value)
                        .unwrap()
                        .clone();
                    return (*native)(args, self.clone().prepare_syms(syms));
                };

                let Expr::Lambda(implementation) = implementation else {
                    panic!("invalid ast");
                };

                let lambda = implementation;
                for (pos, arg) in func.args.iter().cloned().enumerate() {
                    match arg {
                        crate::ast::Argument::Positional(arg_expr) => {
                            let arg_sym = lambda.args[pos].clone();
                            self.push_var(arg_sym.value, arg_expr);
                        }
                        crate::ast::Argument::Named(named_argument) => {
                            self.push_var(named_argument.name.value, *named_argument.value);
                        }
                    }
                }

                // TODO: can we get rid of this clone?
                let body: Box<Runnable> = lambda.clone().body;
                let result = self.run(*body, syms);

                for arg in lambda.clone().args {
                    self.pop_var(arg.value);
                }

                result.nowrap()
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
                        .map(|arg| arg.value.clone())
                        .collect::<Vec<_>>(),
                );

                let mut lambda_runner = self.clone();

                let body = *lambda.clone().body;

                let value = LazyLambda::new(Arc::new(Mutex::new(move |args: Vec<_>| {
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

                Lazy::new_lambda(value)
            }
        }
    }

    fn push_var(&mut self, sym: String, val: Runnable) {
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

    fn read_var(&mut self, sym: String) -> Runnable {
        let stack = self
            .state
            .get_mut(&sym)
            .expect(&format!("read_var: invalid stack: {sym}"));

        stack
            .read()
            .expect(&format!("read_var: invalid stack: {sym}"))
    }
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
