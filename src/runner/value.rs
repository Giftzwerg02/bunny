use std::cell::LazyCell;
use std::hash::Hash;
use std::sync::Arc;

use im::HashMap;
use im::Vector;
use imstr::data::Data;
use imstr::ImString;
use palette::Srgba;

pub type ClonableLazy<'a> = Arc<Lazy<'a>>;

#[derive(Debug, Clone)]
pub enum Lazy<'a> {
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
    Array(Arc<LazyCell<Vector<ClonableLazy<'a>>, Box<dyn FnOnce() -> Vector<ClonableLazy<'a>> + 'a>>>),

    // Keys are eagerly evaluated, but values are lazy
    // TODO Maybe restrict to only "reasoably" hashable keys? int, string, color?
    Dict(
        Arc<LazyCell<
            HashMap<Value<'a>, ClonableLazy<'a>>,
            Box<dyn FnOnce() -> HashMap<Value<'a>, ClonableLazy<'a>> + 'a>,
        >>,
    ),

    Wrapper(
        Arc<LazyCell<Lazy<'a>, Box<dyn FnOnce() -> Lazy<'a> + 'a>>>,
    )
}

impl<'a> Lazy<'a> {
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

    pub fn new_array(value: Vector<ClonableLazy<'a>>) -> Self {
        let callback: Box<dyn FnOnce() -> Vector<ClonableLazy<'a>> + 'a> = Box::new(move || value);
        Lazy::Array(Arc::new(LazyCell::new(callback)))
    }

    pub fn new_dict(value: HashMap<Value<'a>, ClonableLazy<'a>>) -> Self {
        let callback: Box<dyn FnOnce() -> HashMap<Value<'a>, ClonableLazy<'a>> + 'a> =
            Box::new(move || value);
        Lazy::Dict(Arc::new(LazyCell::new(callback)))
    }

    pub fn wrap(f: Box<dyn Fn() -> Lazy<'a> + 'a>) -> Self {
        Lazy::Wrapper(Arc::new(LazyCell::new(f)))
    }

    pub fn eval(self) -> Value<'a> {
        match self {
            Lazy::Int(lazy_cell) => Value::Int(**lazy_cell),
            Lazy::Float(lazy_cell) => Value::Float(**lazy_cell),
            Lazy::String(lazy_cell) => { 
                let pain = (*lazy_cell.clone()).clone(); // TODO What the fuck
                Value::String(pain)
            },
            Lazy::Color(lazy_cell) => Value::Color(**lazy_cell),
            Lazy::Opaque(lazy_cell) => todo!("eval lazy opaque"),
            Lazy::Array(lazy_cell) => todo!("eval lazy array"),
            Lazy::Dict(lazy_cell) => {
                todo!("eval lazy dict")
            }
            Lazy::Wrapper(lazy_cell) => {
                let inner = lazy_cell.clone();
                let inner = (*inner).clone();
                inner.eval()
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value<'a> {
    Int(i64),

    Float(f64),

    String(ImString),

    Color(Srgba),

    Array(Vector<Value<'a>>),

    Dict(HashMap<Value<'a>, Value<'a>>),
}

impl<'a> Into<Lazy<'a>> for Value<'a> {
    fn into(self) -> Lazy<'a> {
        match self {
            Value::Int(int) => Lazy::new_int(int),

            Value::Float(float) => Lazy::new_float(float),

            Value::String(string) => Lazy::new_string(string),

            Value::Color(color) => Lazy::new_color(color),

            Value::Array(_) => todo!(),

            Value::Dict(_) => todo!()
        }
    }
}

impl<'a> Hash for Value<'a> {
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
        }
    }
}

impl<'a> Eq for Value<'a> {
    
}
