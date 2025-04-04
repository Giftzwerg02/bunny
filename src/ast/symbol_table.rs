use std::{collections::HashMap, fmt::Display};
use proptest::bool;
use text_trees::StringTreeNode;

use crate::ast::expressions::{FuncCall, Symbol};

use super::{expressions::{Argument, NonEmptyFuncCall}, Expr, ParsedExpr, AST};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolTable<TExpr: Clone> {
    inner: HashMap<Symbol, TExpr>,
}

impl<TExpr: Display + Clone> SymbolTable<TExpr> {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn insert(&mut self, sym: Symbol, value: TExpr) -> bool {
        self.inner.insert(sym, value).is_some()
    }

    pub fn with(&self, sym: Symbol, value: TExpr) -> Self {
        let mut cpy = self.clone();
        cpy.insert(sym, value);
        cpy
    }

    pub fn contains(&self, sym: &Symbol) -> bool {
        self.inner.contains_key(sym)
    }

    pub fn get(&self, sym: &Symbol) -> Option<&TExpr> {
        self.inner.get(sym)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopedExpr<'a> {
    syms: SymbolTable<ScopedExpr<'a>>,
    value: Box<AST<ScopedExpr<'a>>>,
    inner: ParsedExpr<'a>,
}

impl<'a> ScopedExpr<'a> {
    pub fn new(
        syms: SymbolTable<ScopedExpr<'a>>,
        value: AST<ScopedExpr<'a>>,
        inner: ParsedExpr<'a>,
    ) -> Self {
        Self {
            syms,
            value: Box::new(value),
            inner,
        }
    }
}

impl Expr for ScopedExpr<'_> {
    fn pretty_print(&self) -> StringTreeNode {
        todo!()
    }
}

impl Display for ScopedExpr<'_> {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

pub fn scoped_expr_pass<'a>(
    src: ParsedExpr<'a>,
    symbols: &SymbolTable<ScopedExpr<'a>>,
) -> ScopedExpr<'a> {
    let src_copy = src.clone();
    match *src.value {
        AST::Int(val) => ScopedExpr::new(symbols.clone(), AST::Int(val), src_copy),
        AST::Float(val) => ScopedExpr::new(symbols.clone(), AST::Float(val), src_copy),
        AST::String(val) => ScopedExpr::new(symbols.clone(), AST::String(val), src_copy),
        AST::Color(val) => ScopedExpr::new(symbols.clone(), AST::Color(val), src_copy),
        AST::Symbol(symbol) => {
            if !symbols.contains(&symbol) {
                panic!("unknown symbol: \"{symbol}\"");
            }

            ScopedExpr::new(symbols.clone(), AST::Symbol(symbol), src_copy)
        }
        AST::FuncCall(func_call) => {
            fn convert_func_call<'a>(
                f: FuncCall<ParsedExpr<'a>>,
                symbols: &SymbolTable<ScopedExpr<'a>>,
            ) -> (FuncCall<ScopedExpr<'a>>, SymbolTable<ScopedExpr<'a>>) {
                let f = match f {
                    super::expressions::FuncCall::Single(f) => {
                        let Some(func_def) = symbols.get(&f.id.sym) else {
                            panic!("unknown symbol: \"{}\"", &f.id);
                        };

                        // let symbols = if is_def(&f.id.sym) {
                        //     let new_def = func_from_def(f);
                        //     // symbols.with()
                        // };

                        let args = f
                            .args
                            .into_iter()
                            .map(|arg| match arg {
                                super::expressions::Argument::Unnamed(arg) => {
                                    let arg = scoped_expr_pass(arg, symbols);
                                    super::expressions::Argument::Unnamed(arg)
                                }
                                super::expressions::Argument::Named(arg) => {
                                    if !func_def.syms.contains(&arg.id.sym) {
                                        panic!(
                                            "named argument to unknown parameter: \"{}\"",
                                            arg.id
                                        );
                                    }

                                    let expr = scoped_expr_pass(arg.expr, symbols);
                                    super::expressions::Argument::Named(
                                        super::expressions::NamedArgument::new(arg.id.into(), expr),
                                    )
                                }
                            })
                            .collect();

                        super::expressions::FuncCall::Single(NonEmptyFuncCall::new(f.id.into(), args))
                    }
                    super::expressions::FuncCall::Multi(fs) => {
                        let func = todo!();
                        let mut syms = symbols.clone();
                        for f in fs {
                            let f = convert_func_call(f, &syms);
                            syms = f.syms;
                            a.push(f);
                        }

                        super::expressions::FuncCall::Single(func)
                    }
                };
            }

            let func_call = convert_func_call(func_call, symbols);

            ScopedExpr::new(symbols.clone(), AST::FuncCall(func_call), src_copy)
        }
        AST::Argument(argument) => {
            let argument = match argument {
                super::expressions::Argument::Unnamed(arg) => {
                    let arg = scoped_expr_pass(arg, symbols);
                    super::expressions::Argument::Unnamed(arg)
                }
                super::expressions::Argument::Named(arg) => {
                    let Some(func_def) = symbols.get(&arg.id) else {
                        panic!("illegal state");
                    };

                    if !func_def.syms.contains(&arg.id) {
                        panic!("named argument to unknown parameter: \"{}\"", arg.id);
                    }

                    let expr = scoped_expr_pass(arg.expr, symbols);
                    super::expressions::Argument::Named(super::expressions::NamedArgument::new(
                        arg.id, expr,
                    ))
                }
            };

            ScopedExpr::new(symbols.clone(), AST::Argument(argument), src_copy)
        }
        AST::Array(items) => {
            let items = items
                .into_iter()
                .map(|f| scoped_expr_pass(f, &symbols.clone()))
                .collect();
            ScopedExpr::new(symbols.clone(), AST::Array(items), src_copy)
        }
        AST::Dict(items) => {
            let items = items
                .into_iter()
                .map(|(key, value)| {
                    let key = scoped_expr_pass(key, &symbols.clone());
                    let value = scoped_expr_pass(value, &symbols.clone()); 
                    (key, value)
                })
                .collect();
            ScopedExpr::new(symbols.clone(), AST::Dict(items), src_copy)
        }
    }
}

fn func_from_def<'a>(f: NonEmptyFuncCall<ParsedExpr<'a>>) -> NonEmptyFuncCall<ScopedExpr<'a>> {
    assert!(f.args.len() == 2 || f.args.len() == 3, "illegal amount of arguments for def: (def <name> <params>? <body>)");

    let super::expressions::Argument::Unnamed(id) = f.args[0] else {
        panic!("illegal def function name");
    };

    let AST::Symbol(id) = *id.value else {
        panic!("illegal def function name");
    };

    let params = extract_params(f.args[1]);
    let mut symbols = SymbolTable::new();
    let mut args = vec![];

    for (sym, expr) in params {
        let p_expr = ScopedExpr::new(symbols.clone(), AST::Symbol(sym), expr);
        args.push(Argument::Unnamed(p_expr.clone()));
        if symbols.insert(sym, p_expr) {
            panic!("duplicate paramter: \"{sym}\"");
        }
    }


    let Argument::Unnamed(body) = f.args[2] else {
        panic!("illegal body");
    };
    let body = Argument::Unnamed(scoped_expr_pass(body, &symbols));
    args.push(body);

    NonEmptyFuncCall::new(id, args)
}

fn is_def(id: &Symbol) -> bool {
    id.value == "def"
}

fn extract_params<'a>(params: Argument<ParsedExpr<'a>>) -> Vec<(Symbol, ParsedExpr<'a>)> {
    let Argument::Unnamed(params) = params else {
        panic!("illegal params");
    };
    
    let AST::FuncCall(FuncCall::Single(params)) = *params.value else {
        panic!("illegal params");
    };

    let mut param_list = vec![(params.id.sym, params.id.expr)];
    for p in params.args {
        let Argument::Unnamed(p_expr) = p else {
            panic!("illegal param");
        };
        
        let AST::Symbol(p) = *p_expr.clone().value else {
            panic!("illegal param");
        };

        param_list.push((p, p_expr));
    }

    param_list
}

impl <'a> From<Id<ParsedExpr<'a>>> for Id<ScopedExpr<'a>> {
    fn from(value: Id<ParsedExpr<'a>>) -> Self {
        let syms = SymbolTable::new();
        let expr = ScopedExpr::new(syms, AST::Symbol(value.sym.clone()), value.expr);
        Id::new(value.sym, expr)
    }
}

