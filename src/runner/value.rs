use std::cell::LazyCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;
use std::sync::Mutex;

use palette::Srgba;

use crate::library::runnable_expression::InterpreterSymbolTable;

use super::Runnable;
use super::Runner;

pub type LambdaFunc = dyn FnMut(Vec<Lazy>) -> Lazy;
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
#[derive(Clone)]
pub struct LazyFunc {
    inner: Arc<Mutex<dyn FnMut() -> Lazy>>
}

impl LazyFunc {
    pub fn new(inner: Arc<Mutex<dyn FnMut() -> Lazy>>) -> Self {
        Self {
            inner
        }
    }

    pub fn call(self) -> Lazy {
        let mut i = self.inner.lock().unwrap();
        i()
    }
}

impl Debug for LazyFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "lazyfunc")
    }
}

pub type LazyIterInner = Arc<Mutex<Box<dyn Iterator<Item = Lazy>>>>;
#[derive(Clone)]
pub struct LazyIter {
    pub inner: LazyIterInner,
}

impl LazyIter {
    pub fn new(inner: LazyIterInner) -> Self {
        Self { inner }
    }
}

impl Debug for LazyIter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LazyIter")
    }
}

#[derive(Debug, Clone)]
pub enum Lazy {
    Int(Arc<LazyCell<i64, Box<dyn FnOnce() -> i64>>>),

    Float(Arc<LazyCell<f64, Box<dyn FnOnce() -> f64>>>),

    String(Arc<LazyCell<String, Box<dyn FnOnce() -> String>>>),

    // palette does not seem to have a "general" color type
    // so we just store it as a linear RGBA color for now
    Color(Arc<LazyCell<Srgba, Box<dyn FnOnce() -> Srgba>>>),

    // TODO: Use https://docs.rs/unsvg/latest/unsvg/ or https://docs.rs/esvg/latest/esvg/ for svgs
    Opaque(Arc<LazyCell<()>>),

    // To make it threat safe: Use LazyLock instead of LazyCell and use the
    // thread safe version of the im crate
    Array(Arc<LazyCell<LazyIter, Box<dyn FnOnce() -> LazyIter>>>),

    // Keys are eagerly evaluated, but values are lazy
    // TODO Maybe restrict to only "reasoably" hashable keys? int, string, color?
    Dict(
        Arc<
            LazyCell<
                HashMap<Value, LazyFunc>,
                Box<dyn FnOnce() -> HashMap<Value, LazyFunc>>,
            >,
        >,
    ),

    Wrapper(
        Arc<LazyCell<Lazy, Box<dyn FnOnce() -> Lazy>>>,
    ),
    Lambda(Arc<LazyCell<LazyLambda, Box<dyn FnOnce() -> LazyLambda>>>),
}

impl Lazy {
    pub fn new_int(value: i64) -> Self {
        let callback: Box<dyn FnOnce() -> i64> = Box::new(move || value);
        Lazy::Int(Arc::new(LazyCell::new(callback)))
    }

    pub fn new_float(value: f64) -> Self {
        let callback: Box<dyn FnOnce() -> f64> = Box::new(move || value);
        Lazy::Float(Arc::new(LazyCell::new(callback)))
    }

    pub fn new_string(value: String) -> Self {
        let callback: Box<dyn FnOnce() -> String> = Box::new(move || value);
        Lazy::String(Arc::new(LazyCell::new(callback)))
    }

    pub fn new_color(value: Srgba) -> Self {
        let callback: Box<dyn FnOnce() -> Srgba> = Box::new(move || value);
        Lazy::Color(Arc::new(LazyCell::new(callback)))
    }

    pub fn new_array(value: LazyIter) -> Self {
        let callback: Box<dyn FnOnce() -> LazyIter> = Box::new(move || value);
        Lazy::Array(Arc::new(LazyCell::new(callback)))
    }

    pub fn new_dict(value: HashMap<Value, LazyFunc>) -> Self {
        let callback: Box<dyn FnOnce() -> HashMap<Value, LazyFunc>> = Box::new(move || value);
        Lazy::Dict(Arc::new(LazyCell::new(callback)))
    }

    pub fn new_lambda(value: LazyLambda) -> Self {
        let callback: Box<dyn FnOnce() -> LazyLambda> = Box::new(move || value);
        Lazy::Lambda(Arc::new(LazyCell::new(callback)))
    }

    pub fn wrap(f: Box<dyn FnOnce() -> Lazy>) -> Self {
        Lazy::Wrapper(Arc::new(LazyCell::new(f)))
    }

    pub fn nowrap(self) -> Self {
        match self {
            Lazy::Wrapper(wrapped) => {
                let a = (*wrapped.clone()).clone();
                a
            },
            _ => {
                self
            }
        }
    }

    pub fn full_eval(self) -> Value {
        match self {
            Lazy::Int(lazy_cell) => Value::Int(**lazy_cell),
            Lazy::Float(lazy_cell) => Value::Float(**lazy_cell),
            Lazy::String(lazy_cell) => {
                let pain = (*lazy_cell.clone()).clone(); // TODO What the fuck
                Value::String(pain)
            }
            Lazy::Color(lazy_cell) => Value::Color(**lazy_cell),
            Lazy::Opaque(_) => todo!("eval lazy opaque"),
            Lazy::Array(lazy_cell) => {
                let arr = lazy_cell;
                let mut arr = arr.inner.lock().unwrap();
                let mut res = vec![];
                for elem in arr.into_iter() {
                    res.push(elem.full_eval());
                }
                Value::Array(res)
            }
            Lazy::Dict(lazy_cell) => {
                let init = (**lazy_cell).clone();
                let dict = init.into_iter()
                        .map(|(k, v)| {
                            let v = v.call().full_eval();
                            (k, v)
                        })
                        .collect();
                Value::Dict(dict)
            }
            Lazy::Lambda(lazy_cell) => Value::Lambda((**lazy_cell).clone()),

            Lazy::Wrapper(_) => {
                let inner = self.nowrap();
                inner.full_eval()
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i64),

    Float(f64),

    String(String),

    Color(Srgba),

    Array(Vec<Value>),

    Dict(HashMap<Value, Value>),

    Lambda(LazyLambda),
}

impl Value {
    pub fn name(&self) -> String {
        match self {
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::String(im_string) => "string",
            Value::Color(alpha) => "color",
            Value::Array(vector) => "array",
            Value::Dict(hash_map) => "dict",
            Value::Lambda(lazy_lambda) => "lambda",
        }.to_string()
    }
}

// TODO: this seems... wrong
// NOTE: It was wrong!
// impl Into<Lazy> for Value {
//     fn into(self) -> Lazy {
//         match self {
//             Value::Int(int) => Lazy::new_int(int),
//
//             Value::Float(float) => Lazy::new_float(float),
//
//             Value::String(string) => Lazy::new_string(string),
//
//             Value::Color(color) => Lazy::new_color(color),
//
//             Value::Array(arr) => {
//                 let arr = arr.into_iter().map(|e| e.into()).collect();
//                 Lazy::new_array(arr)
//             },
//
//             Value::Dict(dict) => {
//                 let dict = dict.into_iter().map(|(k, v)| (k, v.into())).collect();
//                 Lazy::new_dict(dict)
//             },
//
//             Value::Lambda(lambda) => Lazy::new_lambda(lambda),
//         }
//     }
// }

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
                alpha.red.to_bits().hash(state);
                alpha.green.to_bits().hash(state);
                alpha.blue.to_bits().hash(state);
            }
            Value::Array(vector) => vector.hash(state),
            Value::Dict(_) => panic!("let's just don't..."),
            Value::Lambda(_) => panic!("cannot hash lambdas... I think?"),
        }
    }
}

impl Eq for Value {}

impl PartialEq for LazyLambda {
    fn eq(&self, other: &Self) -> bool {
        todo!("compare lambda by reference?")
    }
}

#[macro_export]
macro_rules! lwrap {
    ($body:expr) => {
        Lazy::wrap(Box::new(move || $body))
    };
}
