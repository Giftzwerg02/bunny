use std::fmt::Debug;

use im::HashMap;

use crate::{
    ast::{Expr, StageInfo},
    runner::value::{Lazy, Value},
};

pub type InterpreterSymbolTable<'a, I: StageInfo> = HashMap<String, Expr<I>>;

#[derive(Debug, Clone)]
pub enum RunnableExpr<'a, I: StageInfo> {
    Native(NativeExpr<'a>),

    Bunny(Expr<I>), // Howwwww
}

// NOTE: does this work instead?
pub type NativeExpr<'a> = fn(Vec<Lazy<'a>>) -> Lazy<'a>;
