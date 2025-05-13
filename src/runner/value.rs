use std::cell::LazyCell;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;
use std::sync::Mutex;

use esvg::Element;
use im::HashMap;
use im::Vector;
use imstr::ImString;
use palette::Srgba;

pub type LambdaFunc = dyn FnMut(Vector<Lazy>) -> Lazy;
pub type LambdaFuncWrap = Arc<Mutex<LambdaFunc>>;

#[derive(Clone)]
pub struct LazyLambda {
    pub func: LambdaFuncWrap,
}

impl LazyLambda {
    pub fn new(func: LambdaFuncWrap) -> Self {
        Self { func }
    }
}

impl Debug for LazyLambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(lambda)")
    }
}

// pub type ValueLambda<'a> = fn(Vector<Value<'a>>) -> Value<'a>;
pub type LazyType<T> = Arc<LazyCell<T, Box<dyn FnOnce() -> T>>>;

#[derive(Debug, Clone)]
pub enum Lazy {
    Int(LazyType<i64>),

    Float(LazyType<f64>),

    String(LazyType<ImString>),

    // palette does not seem to have a "general" color type
    // so we just store it as a linear RGBA color for now
    Color(LazyType<Srgba<u8>>),

    Opaque(LazyType<Element>),

    // To make it threat safe: Use LazyLock instead of LazyCell and use the
    // thread safe version of the im crate
    Array(LazyType<Vector<Lazy>>),

    // Keys are eagerly evaluated, but values are lazy
    Dict(LazyType<HashMap<Value, Lazy>>),

    Lambda(LazyType<LazyLambda>),
}

impl Lazy {
    pub fn eval(self) -> Value {
        match self {
            Lazy::Int(lazy_cell) => Value::Int(**lazy_cell),
            Lazy::Float(lazy_cell) => Value::Float(**lazy_cell),
            Lazy::String(lazy_cell) => {
                let pain = (*lazy_cell.clone()).clone(); // TODO What the fuck
                Value::String(pain)
            }
            Lazy::Color(lazy_cell) => Value::Color(**lazy_cell),
            Lazy::Opaque(lazy_cell) => Value::Opaque((**lazy_cell).clone()),
            Lazy::Array(lazy_cell) => {
                let mut res = vec![];
                for elem in (*lazy_cell).clone() {
                    res.push(elem.eval());
                }
                Value::Array(res.into())
            }
            Lazy::Dict(lazy_cell) => {
                let init = (**lazy_cell).clone();
                let dict = init.into_iter()
                        .map(|(k, v)| (k, v.eval()))
                        .collect();
                Value::Dict(dict)
            }
            Lazy::Lambda(lazy_cell) => Value::Lambda((**lazy_cell).clone()),

            // Lazy::Wrapper(_) => {
            //     let inner = self.nowrap();
            //     inner.eval()
            // }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),

    Float(f64),

    String(ImString),

    Color(Srgba<u8>),

    Opaque(Element),

    Array(Vector<Value>),

    Dict(HashMap<Value, Value>),

    Lambda(LazyLambda),
}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::Int(int) => int.hash(state),
            // NOTE: this is technically not correct but very much good enough
            //       for what we need
            Value::Float(float) => float.to_bits().hash(state),
            Value::String(string) => string.hash(state),
            // NOTE: same here
            Value::Color(alpha) => {
                alpha.red.hash(state);
                alpha.green.hash(state);
                alpha.blue.hash(state);
                alpha.alpha.hash(state);
            }
            Value::Array(vector) => vector.hash(state),
            Value::Dict(hash_map) => hash_map.hash(state),
            Value::Lambda(_) => panic!("cannot hash lambdas... I think?"),
            Value::Opaque(opaque) => {
                opaque.to_pretty_string().hash(state); // Use string representation for hashing
            }
        }
    }
}

impl Eq for Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Color(a), Value::Color(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Dict(a), Value::Dict(b)) => a == b,
            (Value::Lambda(_), Value::Lambda(_)) => false, // Cannot compare lambdas
            (Value::Opaque(a), Value::Opaque(b)) => a.to_pretty_string() == b.to_pretty_string(),
            _ => false,
        }
    }
}

#[macro_export]
macro_rules! lazy {
    ($value:ident, $to:expr) => {{
        match $value {
            Lazy::Int(lazy_cell) => lazy!(Lazy::Int, $to),
            Lazy::Float(lazy_cell) => lazy!(Lazy::Float, $to),
            Lazy::String(lazy_cell) => lazy!(Lazy::String, $to),
            Lazy::Color(lazy_cell) => lazy!(Lazy::Color, $to),
            Lazy::Opaque(lazy_cell) => todo!(),
            Lazy::Array(lazy_cell) => todo!(),
            Lazy::Dict(lazy_cell) => todo!(),
            Lazy::Lambda(lazy_cell) => todo!(),
        } 
    }};
    ($type:path, $value:expr) => {{
        let callback = Box::new(move || $value);
        $type(Arc::new(core::cell::LazyCell::new(callback)))
    }};
}
