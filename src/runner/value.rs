use im::HashMap;
use im::Vector;
use crate::ast::expressions::Color;
use crate::types::typ::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Color(ColorValue),
    Opaque,

    IntArray(Vector<i64>),
    FloatArray(Vector<f64>),
    StringArray(Vector<String>),
    ColorArray(Vector<ColorValue>),
    OpaqueArray,

    IntDict(HashMap<String, i64>),
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

#[derive(Clone, Debug, PartialEq)]
pub struct ColorValue {
    r: u8,
    g: u8,
    b: u8,
}

impl ColorValue {
    pub fn new(r: u8, g: u8, b: u8) -> ColorValue {
        ColorValue { r, g, b }
    }
}