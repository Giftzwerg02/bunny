use std::sync::Arc;

use im::HashMap;

use crate::runner::value::Lazy;

pub type InterpreterSymbolTable = HashMap<String, NativeExpr>;

// NOTE: does this work instead?
pub type NativeExpr = Arc<dyn Fn(Vec<Lazy>) -> Lazy>;
