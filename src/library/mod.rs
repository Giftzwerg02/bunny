use crate::library;
use crate::runner::value::{ColorValue, Value};
use std::collections::HashMap;

pub mod macros;
pub mod runnable_expression;

fn library() -> HashMap<String, Box<dyn Fn(Vec<Value>) -> Value>> {
    let lib = library! {
        /////////////////////////////////////////////////
        // Boolean Operations
        /////////////////////////////////////////////////
        fn "true" ([]) {
            Value::Int(1)
        }

        fn "false" ([]) {
            Value::Int(0)
        }

        fn "and" ([Value::Int(a), Value::Int(b)]) {
            Value::Int((*a != 0 && *b != 0) as i64)
        }

        fn "or" ([Value::Int(a), Value::Int(b)]) {
            Value::Int((*a != 0 || *b != 0) as i64)
        }

        fn "xor" ([Value::Int(a), Value::Int(b)]) {
            Value::Int(((*a != 0) ^ (*b != 0)) as i64)
        }

        fn "not" ([Value::Int(a)]) {
            Value::Int((*a == 0) as i64)
        }

        /////////////////////////////////////////////////
        // Integer Operations
        /////////////////////////////////////////////////
        fn "add" ([Value::Int(a), Value::Int(b)]) {
            Value::Int(a + b)
        }

        fn "sub" ([Value::Int(a), Value::Int(b)]) {
            Value::Int(a - b)
        }

        fn "mul" ([Value::Int(a), Value::Int(b)]) {
            Value::Int(a * b)
        }

        fn "div" ([Value::Int(a), Value::Int(b)]) {
            Value::Int(a / b)
        }

        fn "mod" ([Value::Int(a), Value::Int(b)]) {
            Value::Int(a % b)
        }

        /////////////////////////////////////////////////
        // Float Operations
        /////////////////////////////////////////////////
         fn "fadd" ([Value::Float(a), Value::Float(b)]) {
            Value::Float(a + b)
        }

        fn "fsub" ([Value::Float(a), Value::Float(b)]) {
            Value::Float(a - b)
        }

        fn "fmul" ([Value::Float(a), Value::Float(b)]) {
            Value::Float(a * b)
        }

        fn "fdiv" ([Value::Float(a), Value::Float(b)]) {
            Value::Float(a / b)
        }

        /////////////////////////////////////////////////
        // String Operations
        /////////////////////////////////////////////////
        fn "concat" ([Value::String(a), Value::String(b)]) {
            Value::String(format!("{}{}", a, b))
        }

        fn "slen" ([Value::String(s)]) {
            Value::Int(s.len() as i64)
        }

        /////////////////////////////////////////////////
        // Colors
        /////////////////////////////////////////////////
        fn "black" ([]) {
            Value::Color(ColorValue::new(0, 0, 0))
        }

        fn "red" ([]) {
            Value::Color(ColorValue::new(255, 0, 0))
        }

        fn "green" ([]) {
            Value::Color(ColorValue::new(0, 255, 0))
        }

        fn "blue" ([]) {
            Value::Color(ColorValue::new(0, 0, 255))
        }

        fn "yellow" ([]) {
            Value::Color(ColorValue::new(255, 255, 0))
        }

        fn "cyan" ([]) {
            Value::Color(ColorValue::new(0, 255, 255))
        }

        fn "magenta" ([]) {
            Value::Color(ColorValue::new(255, 0, 255))
        }

        fn "white" ([]) {
            Value::Color(ColorValue::new(255, 255, 255))
        }

        fn "gray" ([]) {
            Value::Color(ColorValue::new(128, 128, 128))
        }

        /////////////////////////////////////////////////
        // Array Operations
        /////////////////////////////////////////////////
        fn "prepend" ([Value::IntArray(array), Value::Int(value)]) {
            let mut clone = array.clone()
            clone.push_front(*value)

            Value::IntArray(clone)
        }

        fn "append" ([Value::IntArray(array), Value::Int(value)]) {
            let mut clone = array.clone()
            clone.push_back(*value)

            Value::IntArray(clone)
        }

        /////////////////////////////////////////////////
        // Dict Operations
        /////////////////////////////////////////////////
        fn "put" ([Value::IntDict(dict), Value::String(key), Value::Int(value)]) {
            let clone = dict.update(key.clone(), *value)

            Value::IntDict(clone)
        }

        fn "get" ([Value::IntDict(dict), Value::String(key)]) {
            Value::Int(*dict.get(key).unwrap())
        }
    };

    lib
}
