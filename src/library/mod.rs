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
        #[| a:int_type() => b:int_type() => r:int_type()]
        fn "+"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(*a + *b)
        }

        #[| a:int_type() => b:int_type() => r:int_type()]
        fn "-"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(*a - *b)
        }

        #[forall a | inp:array_type(a.clone()) => ret:a ]
        fn "get"(Lazy::List(v)) {
            unimplemented!("get implementation pending")
        }
    }
}

