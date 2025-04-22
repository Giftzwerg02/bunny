#[macro_export]
macro_rules! library {
    ( $(
         #[ $(forall $($var:ident),+ )? | $( $name:ident : $ty:expr ) => + ]
         fn $func_name:tt ( $($arg_pat:pat),* ) $body:block
       )* ) => {{
            let mut scoped: $crate::ast::scoped::SymbolTable<$crate::ast::scoped::ScopedStageInfo> = $crate::ast::scoped::SymbolTable::new();
            let mut typed = $crate::types::InferenceState::new();

            scoped.insert("def".to_string(), $crate::ast::scoped::SymbolValue::Defined);
            scoped.insert("\\".to_string(), $crate::ast::scoped::SymbolValue::Defined);

            $(
                let info = $crate::ast::scoped::ScopedStageInfo::libinfo(SymbolTable::new());

                let call = $crate::ast::FuncCallSingle::new(
                    $crate::ast::Symbol::new("".to_owned(), info.clone()),
                    vec![],
                    info.clone(),
                );

                scoped.insert(
                    stringify!($func_name).trim_matches('"').to_string(),
                    $crate::ast::Lambda::constant( // TODO handle arguments
                        $crate::ast::Expr::FuncCall($crate::ast::FuncCall::Single(call)),
                        info
                    )
                    .into()
                );

                // Generate let bindings for forall variables if they exist
                $( $(let $var = typed.hm.newvar();)* )?

                // Process the type signature - uses the captured $ty expressions
                let all_types = vec![$($ty),+]; // Collect all types
                // Ensure there's at least a return type
                assert!(!all_types.is_empty(), "Function type signature cannot be empty.");
                let args_ty = all_types[..all_types.len() - 1].to_vec(); // All but the last are args
                // Clone the last element safely
                let ret_ty = all_types.last().expect("Should have at least one type").clone(); // The last one is the return type

                // Use the parsed types here
                let func_type = $crate::types::util::func_type(args_ty, ret_ty)
                    .generalize(&mut typed.hm);

                typed.type_assumptions.insert(
                    stringify!($func_name).trim_matches('"').to_string(),
                    $crate::types::typed::TypedValue::FromLibrary(func_type)
                );

                // TODO: Ignore implementation for now
            )*

            $crate::library::Library { scoped, typed }
       }};
}
