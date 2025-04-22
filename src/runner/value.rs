use std::cell::LazyCell;

use im::HashMap;
use im::Vector;
use imstr::ImString;
use palette::Srgba;

pub enum Lazy<'a> {
    Int(LazyCell<i64, Box<dyn FnOnce() -> i64>>),
    
    Float(LazyCell<f64, Box<dyn FnOnce() -> f64>>),
    
    String(LazyCell<ImString, Box<dyn FnOnce() -> ImString>>),
    
    // palette does not seem to have a "general" color type
    // so we just store it as a linear RGBA color for now
    Color(LazyCell<Srgba, Box<dyn FnOnce() -> Srgba>>),

    // TODO: Use https://docs.rs/unsvg/latest/unsvg/ or https://docs.rs/esvg/latest/esvg/ for svgs
    //Opaque(LazyCell<()>),
    
    // To make it threat safe: Use LazyLock instead of LazyCell and use the
    // thread safe version of the im crate
    Array(
        LazyCell<
            Vector<Lazy<'a>>,
            Box<dyn FnOnce() -> Vector<Lazy<'a>> + 'a>
        >
    ),

    // Keys are eagerly evaluated, but values are lazy
    // TODO Maybe restrict to only "reasoably" hashable keys? int, string, color?
    Dict(
        LazyCell<
            HashMap<Value<'a>, Lazy<'a>>,
            Box<dyn FnOnce() -> HashMap<Value<'a>, Lazy<'a>> + 'a>
        >
    ) 
}

impl<'a> Lazy<'a> {
    pub fn new_int(value: i64) -> Self {
        let callback: Box<dyn FnOnce() -> i64> = Box::new(move || value);
        Lazy::Int(LazyCell::new(callback))
    }

    pub fn new_float(value: f64) -> Self {
        let callback: Box<dyn FnOnce() -> f64> = Box::new(move || value);
        Lazy::Float(LazyCell::new(callback))
    }

    pub fn new_string(value: ImString) -> Self {
        let callback: Box<dyn FnOnce() -> ImString> = Box::new(move || value);
        Lazy::String(LazyCell::new(callback))
    }

    pub fn new_color(value: Srgba) -> Self {
        let callback: Box<dyn FnOnce() -> Srgba> = Box::new(move || value);
        Lazy::Color(LazyCell::new(callback))
    }

    pub fn new_array(value: Vector<Lazy<'a>>) -> Self {
        let callback: Box<dyn FnOnce() -> Vector<Lazy<'a>> + 'a> = Box::new(move || value);
        Lazy::Array(LazyCell::new(callback))
    }

    pub fn new_dict(value: HashMap<Value<'a>, Lazy<'a>>) -> Self {
        let callback: Box<dyn FnOnce() -> HashMap<Value<'a>, Lazy<'a>> + 'a> = Box::new(move || value);
        Lazy::Dict(LazyCell::new(callback))
    }
}

pub enum Value<'a> {
    Int(i64),
    
    Float(f64),
    
    String(String),
    
    Color(Srgba),
    
    Array(Vector<Value<'a>>),

    Dict(HashMap<Value<'a>, Value<'a>>)
}
