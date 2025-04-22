use crate::ast::scoped::{ScopedStageInfo, SymbolTable};
use crate::library;
use crate::types::InferenceState;
use crate::types::util::*;

pub mod macros;

pub struct Library<'a> {
    pub scoped: SymbolTable<ScopedStageInfo<'a>>,
    pub typed: InferenceState<'a>
}

pub fn standard_library<'a>() -> Library<'a> {
    library! {
        #[| a:bint() => b:bint() => ret:bint()]
        fn "+"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(*a + *b)
        }

        #[| a:bint() => b:bint() => ret:bint()]
        fn "-"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(*a - *b)
        }

        #[forall a | arr:barray(&a) => ret:a ]
        fn "get"(Lazy::List(v)) {
            unimplemented!("get implementation pending")
        }

        #[forall a, b | fun:bfunc1(&a, &b) => arr:barray(&a) => ret:b ]
        fn "map"(Lazy::List(v)) {

        }
    }
}

