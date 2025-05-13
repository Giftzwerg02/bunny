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

pub type LambdaFunc<'a> = dyn FnMut(Vector<Lazy<'a>>) -> Lazy<'a> + 'a;
pub type LambdaFuncWrap<'a> = Arc<Mutex<LambdaFunc<'a>>>;

#[derive(Clone)]
pub struct LazyLambda<'a> {
    pub func: LambdaFuncWrap<'a>,
}

impl<'a> LazyLambda<'a> {
    pub fn new(func: LambdaFuncWrap<'a>) -> Self {
        Self { func }
    }
}

impl Debug for LazyLambda<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(lambda)")
    }
}

// pub type ValueLambda<'a> = fn(Vector<Value<'a>>) -> Value<'a>;
pub type LazyType<'a, T> = Arc<LazyCell<T, Box<dyn FnOnce() -> T + 'a>>>;

#[derive(Debug, Clone)]
pub enum Lazy<'a> {
    Int(LazyType<'a, i64>),

    Float(LazyType<'a, f64>),

    String(LazyType<'a, ImString>),

    // palette does not seem to have a "general" color type
    // so we just store it as a linear RGBA color for now
    Color(LazyType<'a, Srgba<u8>>),

    Opaque(LazyType<'a, Element>),

    // To make it threat safe: Use LazyLock instead of LazyCell and use the
    // thread safe version of the im crate
    Array(LazyType<'a, Vector<Lazy<'a>>>),

    // Keys are eagerly evaluated, but values are lazy
    Dict(LazyType<'a, HashMap<Value<'a>, Lazy<'a>>>),

    Lambda(LazyType<'a, LazyLambda<'a>>),
}

impl<'a> Lazy<'a> {
    pub fn eval(self) -> Value<'a> {
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
pub enum Value<'a> {
    Int(i64),

    Float(f64),

    String(ImString),

    Color(Srgba<u8>),

    Opaque(Element),

    Array(Vector<Value<'a>>),

    Dict(HashMap<Value<'a>, Value<'a>>),

    // TODO Use this somehow
    #[allow(dead_code)]
    Lambda(LazyLambda<'a>),
}

impl Value<'_> {
    pub fn is_renderable(&self) -> bool {
        return matches!(self, Value::Opaque(_))
    }
}

impl Hash for Value<'_> {
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

impl Eq for Value<'_> {}

impl PartialEq for Value<'_> {
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
