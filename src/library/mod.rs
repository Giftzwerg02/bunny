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
        #[int_type() => int_type() => int_type()]
        fn "+"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(*a + *b)
        }

        #[int_type() => int_type() => int_type()]
        fn "-"(Lazy::Int(a), Lazy::Int(b)) {
            Lazy::new_int(*a - *b)
        }

        /*#[forall a : list_type(a) => a]
        fn "get"(Lazy::List(v)) {
            unimplemented!("get implementation pending")
        }*/
    }
}

