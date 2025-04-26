use crate::ast::scoped::{ScopedStageInfo, SymbolTable};
use crate::{eval, library};
use crate::library::runnable_expression::InterpreterSymbolTable;
use crate::runner::value::Lazy;
use crate::types::InferenceState;
use crate::types::util::*;

pub mod macros;
pub mod runnable_expression;

pub struct Library<'a> {
    pub scoped: SymbolTable<ScopedStageInfo<'a>>,
    pub typed: InferenceState<'a>,
    pub runnable: InterpreterSymbolTable<'a>
}

pub fn standard_library<'a>() -> Library<'a> {
    library! {
        #[| a:bint() => b:bint() => ret:bint()]
        fn "+"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(eval!(a) + eval!(b))
        }

        #[| a:bint() => b:bint() => ret:bint()]
        fn "-"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(eval!(a) - eval!(b))
        }

        #[forall a | arr:barray(&a) => ret:a ]
        fn "first"(Lazy::Array(v)) {
            unimplemented!("map implementation pending")
        }

        #[forall a, b | fun:bfunc1(&a, &b) => arr:barray(&a) => ret:b ]
        fn "map"(Lazy::Array(v)) {
            unimplemented!("map implementation pending")
        }
    }
}

