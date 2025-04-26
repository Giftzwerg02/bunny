use std::cell::LazyCell;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;
use std::sync::Mutex;

use im::HashMap;
use im::Vector;
use imstr::data::Data;
use imstr::ImString;
use palette::Srgba;

pub type ClonableLazy = Arc<Lazy>;
pub type LambdaFunc = dyn FnMut(Vector<Lazy>) -> Lazy;
pub type LambdaFuncWrap = Arc<Mutex<LambdaFunc>>;

#[derive(Clone)]
pub struct LazyLambda { 
    pub func: LambdaFuncWrap 
}

impl LazyLambda {
    pub fn new(func: LambdaFuncWrap) -> Self {
        Self {
            func
        }
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
    Opaque(Arc<LazyCell<()>>),

    // To make it threat safe: Use LazyLock instead of LazyCell and use the
    // thread safe version of the im crate
    Array(Arc<LazyCell<Vector<ClonableLazy>, Box<dyn FnOnce() -> Vector<ClonableLazy>>>>),

    // Keys are eagerly evaluated, but values are lazy
    // TODO Maybe restrict to only "reasoably" hashable keys? int, string, color?
    Dict(
        Arc<LazyCell<
            HashMap<Value, ClonableLazy>,
            Box<dyn FnOnce() -> HashMap<Value, ClonableLazy>>,
        >>,
    ),

    Lambda(
        Arc<LazyCell<
            LazyLambda, 
            Box<dyn FnOnce() -> LazyLambda>
        >>,
    ),
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

    pub fn new_string(value: ImString) -> Self {
        let callback: Box<dyn FnOnce() -> ImString> = Box::new(move || value);
        Lazy::String(Arc::new(LazyCell::new(callback)))
    }

    pub fn new_color(value: Srgba) -> Self {
        let callback: Box<dyn FnOnce() -> Srgba> = Box::new(move || value);
        Lazy::Color(Arc::new(LazyCell::new(callback)))
    }

    pub fn new_array(value: Vector<ClonableLazy>) -> Self {
        let callback: Box<dyn FnOnce() -> Vector<ClonableLazy>> = Box::new(move || value);
        Lazy::Array(Arc::new(LazyCell::new(callback)))
    }

    pub fn new_dict(value: HashMap<Value, ClonableLazy>) -> Self {
        let callback: Box<dyn FnOnce() -> HashMap<Value, ClonableLazy>> =
            Box::new(move || value);
        Lazy::Dict(Arc::new(LazyCell::new(callback)))
    }

    pub fn new_lambda(value: LazyLambda) -> Self {
        let callback: Box<dyn FnOnce() -> LazyLambda> = Box::new(move || value);
        Lazy::Lambda(Arc::new(LazyCell::new(callback)))
    }

    pub fn eval(self) -> Value {
        match self {
            Lazy::Int(lazy_cell) => Value::Int(**lazy_cell),
            Lazy::Float(lazy_cell) => Value::Float(**lazy_cell),
            Lazy::String(lazy_cell) => { 
                let pain = (*lazy_cell.clone()).clone(); // TODO What the fuck
                Value::String(pain)
            },
            Lazy::Color(lazy_cell) => Value::Color(**lazy_cell),
            Lazy::Opaque(lazy_cell) => todo!("eval lazy opaque"),
            Lazy::Array(lazy_cell) => {
                let mut res = vec![];
                for elem in (*lazy_cell).clone() {
                    res.push((*elem).clone().eval());
                }
                Value::Array(res.into())
            },
            Lazy::Dict(lazy_cell) => {
                        todo!("eval lazy dict")
                    }
            Lazy::Lambda(lazy_cell) => Value::Lambda((**lazy_cell).clone()),
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

    Lambda(LazyLambda)
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
                alpha.red.to_bits().hash(state);
                alpha.green.to_bits().hash(state);
                alpha.blue.to_bits().hash(state);
            },
            Value::Array(vector) => vector.hash(state),
            Value::Dict(hash_map) => hash_map.hash(state),
            Value::Lambda(_) => panic!("cannot hash lambdas... I think?"),
        }
    }
}

impl Eq for Value {
    
}

impl PartialEq for LazyLambda {
    fn eq(&self, other: &Self) -> bool {
        todo!("compare lambda by reference?")
    }
}
