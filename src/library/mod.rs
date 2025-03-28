use std::collections::HashMap;
use crate::library;
use crate::runner::value::Value;

pub mod macros;
pub mod runnable_expression;

fn library() -> HashMap<String, Box<dyn Fn(Vec<Value>) -> Value>> {
    let lib = library! {
         fn "+" ([Value::Int(a), Value::Int(b)]) {
             Value::Int(a + b)
         }

         fn "concat" ([Value::String(a), Value::String(b)]) {
             Value::String(format!("{}{}", a, b))
         }
    };

    lib
}