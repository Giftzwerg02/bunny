use std::sync::Arc;

use im::HashMap;

use crate::runner::value::Lazy;

pub type InterpreterSymbolTable = HashMap<String, NativeExpr>;

pub type NativeExpr = Arc<dyn Fn(Vec<Lazy>) -> Lazy>;
