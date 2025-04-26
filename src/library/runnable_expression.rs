use std::sync::Arc;

use im::HashMap;

use crate::{
    ast::{Expr, StageInfo},
    runner::value::Lazy,
};

pub type InterpreterSymbolTable<'a> = HashMap<String, NativeExpr<'a>>;

pub enum RunnableExpr<'a, I: StageInfo> {
    Native(NativeExpr<'a>),

    Bunny(Expr<I>), // Howwwww
}

// NOTE: does this work instead?
pub type NativeExpr<'a> = Arc<dyn Fn(Vec<Lazy<'a>>) -> Lazy<'a> + 'a>;
