#[macro_export]
macro_rules! library {
    ( $(
         #[ $(forall $($var:ident),+ )? | $( $name:ident : $ty:expr ) => + ]
         fn $func_name:tt ( $($arg_pat:pat),* ) $body:block
       )* ) => {{
            let mut scoped: $crate::ast::scoped::SymbolTable<$crate::ast::scoped::ScopedStageInfo> = $crate::ast::scoped::SymbolTable::new();
            let mut typed = $crate::types::InferenceState::new();
            let mut runnable = $crate::library::InterpreterSymbolTable::new();

            scoped.insert("def".to_string(), $crate::ast::scoped::SymbolValue::Defined);
            scoped.insert("\\".to_string(), $crate::ast::scoped::SymbolValue::Defined);

            $(
                let name = stringify!($func_name).trim_matches('"');
                let info = $crate::ast::scoped::ScopedStageInfo::libinfo(SymbolTable::new());

                let call = $crate::ast::FuncCallSingle::new(
                    $crate::ast::Symbol::new("".to_owned(), info.clone()),
                    vec![],
                    info.clone(),
                );

                scoped.insert(
                    name.to_string(),
                    $crate::ast::Lambda::constant( // TODO handle arguments
                        $crate::ast::Expr::FuncCall($crate::ast::FuncCall::Single(call)),
                        info
                    )
                    .into()
                );

                // Enter a higher level for library function type variable creation
                typed.hm.enter_level();
                
                // Generate let bindings for forall variables if they exist
                $( $(let $var = typed.hm.newvar();)* )?

                // Process the type signature - uses the captured $ty expressions
                let all_types = vec![$(($ty).clone()),+]; // Clone each type
                // Ensure there's at least a return type
                assert!(!all_types.is_empty(), "Function type signature cannot be empty.");
                let args_ty = all_types[..all_types.len() - 1].to_vec(); // All but the last are args
                // Clone the last element safely
                let ret_ty = all_types.last().expect("Should have at least one type").clone(); // The last one is the return type

                // Build the function type
                let func_typ = $crate::types::util::func(&args_ty[..], &ret_ty);
                
                // Exit the level before generalization so the type variables get captured
                typed.hm.exit_level();
                
                // Now generalize at the original level
                let func_type = func_typ.generalize(&mut typed.hm);

                typed.type_assumptions.insert(
                    name.to_string(),
                    $crate::types::typed::TypedValue::FromLibrary(func_type)
                );

                // Store the function pointer (type NativeExpr) in the runnable table.
                runnable.insert(
                    name.to_string(),
                    ::std::sync::Arc::new(move |args| { // Use Arc for cloneable shared ownership
                        let args = args
                            .into_iter()
                            .collect::<::std::vec::Vec<$crate::library::Lazy>>();


                        match &args[..] {
                            [ $($arg_pat,)* ] => $body,
                            _ => panic!("Invalid argument count or types for function '{}'\nargs: {:?}", name, args),
                        }
                    })
                );
            )*

            $crate::library::Library { scoped, typed, runnable }
       }};
}

#[macro_export]
macro_rules! eval {
    ($arg:expr) => {
        (**$arg).clone()
    };
}
