use std::collections::HashMap;
use crate::ast::expressions::Color;
use crate::types::typ::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(u64),
    Float(f64),
    String(String),
    Color(u32),
    Opaque,

    IntArray(Vec<u64>),
    FloatArray(Vec<f64>),
    StringArray(Vec<String>),
    ColorArray(Vec<u32>),
    OpaqueArray,

    IntDict(HashMap<String, u64>),
    FloatDict(HashMap<String, f64>),
    StringDict(HashMap<String, String>),
    ColorDict(HashMap<String, Color>),
    OpaqueDict,
}


impl Value {
    pub fn to_type(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::String(_) => Type::String,
            Value::Color(_) => Type::Color,
            Value::Opaque => Type::Opaque,

            Value::IntArray(_) => Type::IntArray,
            Value::FloatArray(_) => Type::FloatArray,
            Value::StringArray(_) => Type::StringArray,
            Value::ColorArray(_) => Type::ColorArray,
            Value::OpaqueArray => Type::OpaqueArray,

            Value::IntDict(_) => Type::IntDict,
            Value::FloatDict(_) => Type::FloatDict,
            Value::StringDict(_) => Type::StringDict,
            Value::ColorDict(_) => Type::ColorDict,
            Value::OpaqueDict => Type::OpaqueDict,
        }
    }
}