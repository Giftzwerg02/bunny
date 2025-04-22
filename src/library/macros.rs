#[macro_export]
macro_rules! library {
    ( $(
         #[func_type($args_ty:expr, $ret_ty:expr)]
         fn $func_name:tt ( $($arg_pat:pat),* ) -> $ret_expr_ty:ty $body:block
       )* ) => {{
            let mut scoped: $crate::ast::scoped::SymbolTable<$crate::ast::scoped::ScopedStageInfo> = $crate::ast::scoped::SymbolTable::new();
            let mut typed: $crate::types::typed::TypedSymbolTable = $crate::types::typed::TypedSymbolTable::new();

            scoped.insert("def".to_string(), $crate::ast::scoped::SymbolValue::Defined);
            scoped.insert("\\".to_string(), $crate::ast::scoped::SymbolValue::Defined);

            let mut inference_state = $crate::types::InferenceState::new();

            $(
                scoped.insert(
                    stringify!($func_name).to_string(),
                    $crate::ast::scoped::SymbolValue::Defined
                );

                let func_type = $crate::types::util::func_type($args_ty, $ret_ty)
                    .generalize(&mut inference_state.hm);

                typed.insert(
                    stringify!($func_name).to_string(),
                    $crate::types::typed::TypedValue::FromLibrary(func_type)
                );

                // TODO: Ignore implementation for now
            )*

            $crate::library::Library { scoped, typed }
       }};
}
