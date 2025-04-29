use std::cell::LazyCell;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;
use std::sync::Mutex;

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

#[derive(Debug, Clone)]
pub enum Lazy {
    Int(Arc<LazyCell<i64, Box<dyn FnOnce() -> i64>>>),

    Float(Arc<LazyCell<f64, Box<dyn FnOnce() -> f64>>>),

    String(Arc<LazyCell<ImString, Box<dyn FnOnce() -> ImString>>>),

    // palette does not seem to have a "general" color type
    // so we just store it as a linear RGBA color for now
    Color(Arc<LazyCell<Srgba, Box<dyn FnOnce() -> Srgba>>>),

    // TODO: Use https://docs.rs/unsvg/latest/unsvg/ or https://docs.rs/esvg/latest/esvg/ for svgs
    Opaque(Arc<LazyCell<(), Box<dyn FnOnce() -> ()>>>),

    // To make it threat safe: Use LazyLock instead of LazyCell and use the
    // thread safe version of the im crate
    Array(Arc<LazyCell<Vector<Lazy>, Box<dyn FnOnce() -> Vector<Lazy>>>>),

    // Keys are eagerly evaluated, but values are lazy
    // TODO Maybe restrict to only "reasoably" hashable keys? int, string, color?
    Dict(
        Arc<
            LazyCell<
                HashMap<Value, Lazy>,
                Box<dyn FnOnce() -> HashMap<Value, Lazy>>,
            >,
        >,
    ),

    // Wrapper(
    //     Arc<LazyCell<Lazy, Box<dyn FnOnce() -> Lazy>>>,
    // ),
    Lambda(Arc<LazyCell<LazyLambda, Box<dyn FnOnce() -> LazyLambda>>>),
}

impl Lazy {
    // pub fn new_int(value: i64) -> Self {
    //     let callback: Box<dyn FnOnce() -> i64> = Box::new(move || value);
    //     Lazy::Int(Arc::new(LazyCell::new(callback)))
    // }
    //
    // pub fn new_float(value: f64) -> Self {
    //     let callback: Box<dyn FnOnce() -> f64> = Box::new(move || value);
    //     Lazy::Float(Arc::new(LazyCell::new(callback)))
    // }
    //
    // pub fn new_string(value: ImString) -> Self {
    //     let callback: Box<dyn FnOnce() -> ImString> = Box::new(move || value);
    //     Lazy::String(Arc::new(LazyCell::new(callback)))
    // }
    //
    // pub fn new_color(value: Srgba) -> Self {
    //     let callback: Box<dyn FnOnce() -> Srgba> = Box::new(move || value);
    //     Lazy::Color(Arc::new(LazyCell::new(callback)))
    // }
    //
    // pub fn new_array(value: Vector<Lazy>) -> Self {
    //     let callback: Box<dyn FnOnce() -> Vector<Lazy>> = Box::new(move || value);
    //     Lazy::Array(Arc::new(LazyCell::new(callback)))
    // }
    //
    // pub fn new_dict(value: HashMap<Value, Lazy>) -> Self {
    //     let callback: Box<dyn FnOnce() -> HashMap<Value, Lazy>> = Box::new(move || value);
    //     Lazy::Dict(Arc::new(LazyCell::new(callback)))
    // }
    //
    // pub fn new_lambda(value: LazyLambda) -> Self {
    //     let callback: Box<dyn FnOnce() -> LazyLambda> = Box::new(move || value);
    //     Lazy::Lambda(Arc::new(LazyCell::new(callback)))
    // }

    // pub fn wrap(f: Box<dyn FnOnce() -> Lazy>) -> Self {
    //     Lazy::Wrapper(Arc::new(LazyCell::new(f)))
    // }
    //
    // pub fn nowrap(self) -> Self {
    //     match self {
    //         Lazy::Wrapper(wrapped) => {
    //             let a = (*wrapped.clone()).clone();
    //             a
    //         },
    //         _ => {
    //             self
    //         }
    //     }
    // }

    pub fn eval(self) -> Value {
        match self {
            Lazy::Int(lazy_cell) => Value::Int(**lazy_cell),
            Lazy::Float(lazy_cell) => Value::Float(**lazy_cell),
            Lazy::String(lazy_cell) => {
                let pain = (*lazy_cell.clone()).clone(); // TODO What the fuck
                Value::String(pain)
            }
            Lazy::Color(lazy_cell) => Value::Color(**lazy_cell),
            Lazy::Opaque(lazy_cell) => todo!("eval lazy opaque"),
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

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i64),

    Float(f64),

    String(ImString),

    Color(Srgba),

    Array(Vector<Value>),

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
            Value::Dict(hash_map) => hash_map.hash(state),
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

// #[macro_export]
// macro_rules! lwrap {
//     ($body:expr) => {
//         Lazy::wrap(Box::new(move || $body))
//     };
// }

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

// fn a(val: Lazy) {
//     match val {
//         Lazy::Int(lazy_cell) => lazy!(Lazy::Int, $value),
//         Lazy::Float(lazy_cell) => lazy!(Lazy::Float, $value),
//         Lazy::String(lazy_cell) => todo!(),
//         Lazy::Color(lazy_cell) => todo!(),
//         Lazy::Opaque(lazy_cell) => todo!(),
//         Lazy::Array(lazy_cell) => todo!(),
//         Lazy::Dict(lazy_cell) => todo!(),
//         Lazy::Lambda(lazy_cell) => todo!(),
//     }
// }
