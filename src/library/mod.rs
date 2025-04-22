use crate::ast::scoped::{ScopedStageInfo, SymbolTable};
use crate::library;
use crate::types::InferenceState;
use crate::types::typed::TypedSymbolTable;
use crate::types::util::int_type;

pub mod macros;

pub struct Library<'a> {
    pub scoped: SymbolTable<ScopedStageInfo<'a>>, // Made public for access
    pub typed: InferenceState<'a> // Made public for access
}

pub fn standard_library<'a>() -> Library<'a> {
    library! {
        #[func_type(vec![int_type(), int_type()], int_type())]
        fn "+"(Lazy::Int(a), Lazy::Int(b)) -> Lazy {
            Lazy::new_int(a + b)
        }

        #[func_type(vec![int_type(), int_type()], int_type())]
        fn "-"(Lazy::Int(a), Lazy::Int(b)) -> Lazy {
            Lazy::new_int(a - b)
        }
    }
}
