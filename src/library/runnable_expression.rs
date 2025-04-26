use std::sync::Arc;

use im::HashMap;

use crate::{
    ast::{Expr, StageInfo},
    runner::value::Lazy,
};

pub type InterpreterSymbolTable = HashMap<String, NativeExpr>;

pub enum RunnableExpr<I: StageInfo> {
    Native(NativeExpr),

    Bunny(Expr<I>), // Howwwww
}

// NOTE: does this work instead?
pub type NativeExpr = Arc<dyn Fn(Vec<Lazy>) -> Lazy>;
