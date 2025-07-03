use std::cell::LazyCell;
use std::cell::RefCell;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;

use esvg::Element;
use im::HashMap;
use im::Vector;
use imstr::ImString;
use palette::Alpha;
use palette::Srgb;
use palette::Srgba;

use crate::types::hm::Type;
use crate::types::hm::TypeVar;

pub type LambdaFunc = dyn FnMut(Vector<Lazy>) -> Lazy;
pub type LambdaFuncWrap = Rc<RefCell<LambdaFunc>>;

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
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(lambda)")
    }
}

pub type LazyType<T> = Rc<LazyCell<T, Box<dyn FnOnce() -> T>>>;

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
                let pain = (**lazy_cell).clone();
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
                let dict = init.into_iter().map(|(k, v)| (k, v.eval())).collect();
                Value::Dict(dict)
            }
            Lazy::Lambda(_) => Value::Lambda(),
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

    Lambda(),
}

impl Value {
    pub fn is_renderable(&self) -> bool {
        matches!(self, Value::Opaque(_))
    }
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
            Value::Lambda() => panic!("cannot hash lambdas... I think?"),
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
            (Value::Lambda(), Value::Lambda()) => false, // Cannot compare lambdas
            (Value::Opaque(a), Value::Opaque(b)) => a.to_pretty_string() == b.to_pretty_string(),
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Int(int) => write!(f, "{int}"),
            Value::Float(float) => write!(f, "{float}"),
            Value::String(string) => write!(f, "{string}"),
            Value::Color(color) => write!(f, "{}", to_color_str(color)),
            Value::Array(array) => write!(
                f,
                "[{}]",
                array
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Value::Dict(dict) => write!(
                f,
                "[{}]",
                dict.iter()
                    .map(|(k, v)| format!("{k}: {v}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Value::Lambda() => write!(f, "(lambda)"),
            Value::Opaque(opaque) => write!(f, "{}", opaque.to_pretty_string()),
        }
    }
}

pub fn to_color_str(color: &Alpha<Srgb<u8>, u8>) -> String {
    let (r, g, b, a) = color.into_components();
    format!("#{r:02x}{g:02x}{b:02x}{a:02x}")
}

pub enum LT {
    Int,
    Float,
    String,
    Color,
    Opaque,
    Array,
    Dict,
    Lambda,
}

pub fn resolve_to_lazy_type(t: Type) -> LT {
    fn to_concrete(t: Type) -> Type {
        let Type::TVar(tvar) = t else {
            panic!("invalid type parameter");
        };

        let TypeVar::Bound(bound) = tvar.borrow().clone() else {
            panic!("invalid typevar bound: {tvar:?}");
        };

        match bound {
            Type::TVar(_) => to_concrete(bound),
            Type::TUnit => panic!("invalid type bound"),
            _ => bound,
        }
    }

    let concrete = to_concrete(t);
    match concrete {
        crate::types::hm::Type::Basic(basic) => match basic.as_ref() {
            "int" => LT::Int,
            "float" => LT::Float,
            "string" => LT::String,
            "color" => LT::Color,
            "opaque" => LT::Opaque,
            _ => panic!("invalid basic type"),
        },
        crate::types::hm::Type::TApp(app, _) => match app.as_ref() {
            "array" => LT::Array,
            "dict" => LT::Dict,
            _ => panic!("invalid app type"),
        },
        crate::types::hm::Type::Fn(_, _) => LT::Lambda,
        _ => panic!("invalid type bound: {concrete:?}"),
    }
}

#[macro_export]
macro_rules! lazy {
    ([$($value:ident -> $into:ident),+], $to:expr) => {{
       match ($($value),+) {
            #[allow(unused_parens)]
            ($(Lazy::Int($into)),+) => {
                lazy!(Lazy::Int, $to)
            }
            #[allow(unused_parens)]
            ($(Lazy::Float($into)),+) => {
                lazy!(Lazy::Float, $to)
            }
            #[allow(unused_parens)]
            ($(Lazy::String($into)),+) => {
                lazy!(Lazy::String, $to)
            }
            #[allow(unused_parens)]
            ($(Lazy::Color($into)),+) => {
                lazy!(Lazy::Color, $to)
            }
            #[allow(unused_parens)]
            ($(Lazy::Opaque($into)),+) => {
                lazy!(Lazy::Opaque, $to)
            }
            #[allow(unused_parens)]
            ($(Lazy::Array($into)),+) => {
                lazy!(Lazy::Array, $to)
            }
            #[allow(unused_parens)]
            ($(Lazy::Dict($into)),+) => {
                lazy!(Lazy::Dict, $to)
            }
            #[allow(unused_parens)]
            ($(Lazy::Lambda($into)),+) => {
                lazy!(Lazy::Lambda, $to)
            },
            #[allow(unreachable_patterns)]
            _ => panic!("illegal variadic return type")
        }
    }};
    ($type:path, $value:expr) => {{
        let callback = Box::new(move || $value);
        $type(::std::rc::Rc::new(core::cell::LazyCell::new(callback)))
    }};
    (fromtype[$t:ident], $value:expr) => {{
            let lt = $crate::runner::value::resolve_to_lazy_type($t.clone());
            match lt {
                $crate::runner::value::LT::Int => lazy!(Lazy::Int, $value),
                $crate::runner::value::LT::Float => lazy!(Lazy::Float, $value),
                $crate::runner::value::LT::String => lazy!(Lazy::String, $value),
                $crate::runner::value::LT::Color => lazy!(Lazy::Color, $value),
                $crate::runner::value::LT::Opaque => lazy!(Lazy::Opaque, $value),
                $crate::runner::value::LT::Array => lazy!(Lazy::Array, $value),
                $crate::runner::value::LT::Dict => lazy!(Lazy::Dict, $value),
                $crate::runner::value::LT::Lambda => lazy!(Lazy::Lambda, $value),
            }
    }}
}
