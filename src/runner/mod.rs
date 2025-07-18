use std::{
    cell::RefCell, collections::HashMap, rc::Rc, sync::{Arc}
};

use im::Vector;
use palette::Srgba;
use value::{Lazy, LazyLambda, Value};

use crate::{
    ast::{Argument, Expr, FuncCall, FuncCallSingle, Lambda, StageInfo, Symbol},
    lazy,
    library::runnable_expression::InterpreterSymbolTable,
    types::typed::{AnyTypedStageInfo, PolyTypedStageInfo, TypedValue},
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

#[derive(Clone)]
pub struct Runner {
    state: HashMap<String, SymbolStack>,
    native_syms: InterpreterSymbolTable,
}

impl Runner {
    pub fn new(native_syms: InterpreterSymbolTable) -> Self {
        Runner {
            state: HashMap::new(),
            native_syms,
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
    pub fn run(&mut self, expr: Expr<AnyTypedStageInfo>) -> Lazy {
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
                lazy!(
                    Lazy::Color,
                    Srgba::new(color.r, color.g, color.b, color.alpha)
                )
            }
            Expr::Array(array) => {
                self.run_array(array)
            }
            Expr::Dict(dict) => {
                self.run_dict(dict)
            }
            Expr::Symbol(symbol) => {
                self.run_symbol(symbol)
            }
            Expr::FuncCall(func) => {
                let FuncCall::Single(func) = func else {
                    panic!("invalid ast");
                };

                self.run_func_call(func)
            }
            Expr::Lambda(lambda) => {
                self.run_lambda(lambda)
            }
        }
    }

    fn run_array(&mut self, array: crate::ast::Array<AnyTypedStageInfo>) -> Lazy {
        let mut runner = self.clone();
        lazy!(Lazy::Array, {
            let mut res = Vector::new();

            for elem in array.value {
                let elem = runner.run(elem);
                res.push_back(elem);
            }

            res
        })
    }

    fn run_dict(&mut self, dict: crate::ast::Dict<AnyTypedStageInfo>) -> Lazy {
        let mut runner = self.clone();
        lazy!(Lazy::Dict, {
            let mut res: im::HashMap<Value, Lazy> = im::HashMap::new();
            for entry in dict.value {
                let key = runner.run(entry.key).eval();
                let value = runner.run(entry.value);

                res.insert(key, value);
            }
            res
        })
    }

    fn run_symbol(&mut self, symbol: Symbol<AnyTypedStageInfo>) -> Lazy {
        // if the implementation of a function is just a symbol,
        // then we just have another indirection
        //
        // We are inside some func-call, meaning a variable could either come from:
        // 1. An immediately preceeding "let-binding" (def foo ...)
        // 2. From the outer scope which is stored on the SymbolStack
        // 3. A default argument
        //
        // Choosing the value should be treated in this order, i.e. default arguments have
        // the least priority while local let-bindings have the highest priority
        //
        // Therefore, if the symbol resolves to an argument we need to check the
        // SymbolStack
        


        let Some(scope) = symbol.info.syms.get(&symbol.value) else {
            return self.read_var(symbol.value);
        };

        match scope {
            TypedValue::FromLibrary(_) => {
                let symbol_value_owned = symbol.value.clone();
                let native_syms_owned = self.native_syms.clone();

                lazy!(Lazy::Lambda, {
                    let native_fn =
                        native_syms_owned.get(&symbol_value_owned).unwrap().clone();
                    LazyLambda::new(Rc::new(RefCell::new(move |args: Vector<Lazy>| {
                        (*native_fn)(args.into_iter().collect())
                    })))
                })
            }
            TypedValue::FromBunny(scope) => {
                let new_scope = scope.clone().map_stage(&mut |info| info.into());
                self.run(new_scope)
            }
        }
    }

    fn run_func_call(&mut self, func: FuncCallSingle<AnyTypedStageInfo>) -> Lazy {
        let implementation = func.info.syms.get(&func.id.value).expect("scoping err");

        let TypedValue::FromBunny(implementation) = implementation else {
            let args = self.run_func_args(func.clone());
            let native = self.native_syms.get(&func.id.value).unwrap();
            return native(args);
        };

        match implementation {
            // i.e., the argument is used as a function
            Expr::Symbol(symbol) => {
                self.run_func_call_with_symbol(func.clone(), symbol)
            }
            Expr::Lambda(lambda) => {
                self.run_func_call_with_lambda(func.clone(), lambda)
            }
            thing => {
                self.run_func_call_with_expression(func.clone(), thing)
            }
        }
    }

    fn run_func_call_with_symbol(&mut self, func: FuncCallSingle<AnyTypedStageInfo>, symbol: &Symbol<PolyTypedStageInfo>) -> Lazy {
        let value = Expr::Symbol(symbol.clone())
            .clone()
            .map_stage(&mut |info| info.into());
        let value = self.run(value);
        let Lazy::Lambda(lambda) = value else {
            return value;
        };

        let args = func.args.into_iter().map(|arg| {
            match arg {
                Argument::Positional(expr) => {
                    self.run(expr)
                },
                Argument::Named(_) => {
                    panic!("named arguments are not allowed when passing to an argument-lambda")
                },
            }
        }).collect();

        let lambda_call = &mut lambda.func.borrow_mut();
        lambda_call(args)
    }

    fn run_func_call_with_lambda(&mut self, func: FuncCallSingle<AnyTypedStageInfo>, lambda: &Lambda<PolyTypedStageInfo>) -> Lazy {
        let mut lambda_set_args = lambda.clone().map_stage(&mut |info| info.into());
        let lambda_pop_args = lambda_set_args.clone();

        let mut already_set_arguments = vec![];
        for (pos, arg) in func.args.iter().cloned().enumerate() {
            match arg {
                crate::ast::Argument::Named(named_argument) => {
                    already_set_arguments.push(named_argument.name.clone());
                    let arg_value = self.run(*named_argument.value);
                    self.push_var(named_argument.name.value, arg_value);
                }
                crate::ast::Argument::Positional(arg_expr) => {
                    let arg_sym = lambda.args[pos]
                        .into_def_argument_symbol()
                        .map_stage(&mut |info| info.into());

                    already_set_arguments.push(arg_sym.clone());
                    let arg_value = self.run(arg_expr);
                    self.push_var(arg_sym.value, arg_value);
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
            let default_value = self.run(default_value);
            self.push_var(name.value, default_value);
        }

        let body = lambda.clone().body.map_stage(&mut |info| info.into());

        let result = self.run(body);

        for arg in lambda_pop_args.args {
            self.pop_var(arg.into_def_argument_symbol().value);
        }

        result
    }

    fn run_func_call_with_expression(&mut self, func: FuncCallSingle<AnyTypedStageInfo>, thing: &Expr<crate::types::typed::PolyTypedStageInfo>) -> Lazy {
        let right_thing = thing.clone().map_stage(&mut |info| info.into());

        let Lazy::Lambda(lambda) = self.run(right_thing) else {
            panic!("argument must be a lambda")
        };

        let args = func.args.into_iter().map(|arg| {
            match arg {
                Argument::Positional(expr) => {
                    self.run(expr)
                },
                Argument::Named(_) => {
                    panic!("named arguments are not allowed when passing to an argument-lambda")
                },
            }
        }).collect();

        let lambda_call = &mut lambda.func.borrow_mut();
        lambda_call(args)
    }

    fn run_lambda(&mut self, lambda: crate::ast::Lambda<AnyTypedStageInfo>) -> Lazy {
        // A lambda should return a lazy of
        // a function that takes some arguments and returns a result

        let lambda_args = Arc::new(
            lambda
                .args
                .iter()
                .map(|arg| arg.into_def_argument_symbol().value.clone())
                .collect::<Vec<_>>(),
        );

        let mut lambda_runner = self.clone();
        let body = (*lambda.clone().body).map_stage(&mut |info| info);

        lazy!(Lazy::Lambda, {
            LazyLambda::new(Rc::new(RefCell::new(move |args: Vector<_>| {
                let body = body.clone();
                let lambda_args = (*lambda_args).clone();
                for (pos, arg) in args.iter().cloned().enumerate() {
                    let arg_sym = lambda_args[pos].clone();
                    lambda_runner.push_var(arg_sym, arg);
                }

                let result = lambda_runner.run(body);

                for arg in (lambda_args).clone() {
                    lambda_runner.pop_var(arg);
                }

                result
            })))
        })
    }

    fn run_func_args(&mut self, f: FuncCallSingle<AnyTypedStageInfo>) -> Vec<Lazy> {
        f.args
            .into_iter()
            .map(|arg| match arg {
                crate::ast::Argument::Positional(arg_expr) => {
                    self.run(arg_expr)
                }
                crate::ast::Argument::Named(named_argument) => {
                    self.run(*named_argument.value)
                }
            })
            .collect()
    }

    fn push_var(&mut self, sym: String, val: Lazy) {
        match self.state.get_mut(&sym) {
            Some(stack) => stack.push(val),
            None => {
                let mut stack = SymbolStack::new();
                stack.push(val);
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
            .unwrap_or_else(|| panic!("read_var: invalid stack: {sym}"));

        stack
            .read()
            .unwrap_or_else(|| panic!("read_var: invalid stack: {sym}"))
    }
}

fn pop_arg_by_name<I: StageInfo>(args: &mut Vec<Argument<I>>, name: &Symbol<I>) {
    let idx = args
        .iter()
        .position(|arg| arg.into_def_argument_symbol().value == name.value)
        .expect("called pop_arg_by_name with non-existent symbol-name");
    args.remove(idx);
}
