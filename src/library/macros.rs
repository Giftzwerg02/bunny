#[macro_export]
macro_rules! library {
    ( $(
         #[ $(forall $($var:ident),+ )? | $( $name:ident : $ty:expr ) => + ]
         fn $func_name:tt ( $($arg_pat:pat),* ): ($runner:pat) $body:block
       )* ) => {{
            let mut scoped: $crate::ast::scoped::SymbolTable<$crate::ast::scoped::ScopedStageInfo> = $crate::ast::scoped::SymbolTable::new();
            let mut typed = $crate::types::InferenceState::new();
            let mut runnable: $crate::library::InterpreterSymbolTable = $crate::library::InterpreterSymbolTable::new();

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

                // Generate let bindings for forall variables if they exist
                $( $(let $var = typed.hm.newvar();)* )?

                // Process the type signature - uses the captured $ty expressions
                let all_types = vec![$(($ty).clone()),+]; // Clone each type
                // Ensure there's at least a return type
                assert!(!all_types.is_empty(), "Function type signature cannot be empty.");
                let args_ty = all_types[..all_types.len() - 1].to_vec(); // All but the last are args
                // Clone the last element safely
                let ret_ty = all_types.last().expect("Should have at least one type").clone(); // The last one is the return type

                // Use the parsed types here
                let func_type = $crate::types::util::func(&args_ty[..], &ret_ty)
                    .generalize(&mut typed.hm);

                typed.type_assumptions.insert(
                    name.to_string(),
                    $crate::types::typed::TypedValue::FromLibrary(func_type)
                );

                // Store the function pointer (type NativeExpr) in the runnable table.
                runnable.insert(
                    name.to_string(),
                    ::std::sync::Arc::new(move |args, runner| { // Use Arc for cloneable shared ownership
                        Lazy::wrap(Box::new(move || {
                            match &args[..] {
                                [ $($arg_pat,)* ] => {
                                    match runner {
                                        $runner => $body
                                    }
                                }
                                _ => panic!("Invalid argument count or types for function '{}'\nargs: {:?}", name, args),
                            }
                        }))
                    })
                );
            )*

            $crate::library::Library { scoped, typed, runnable }
       }};
}

#[macro_export]
macro_rules! eval {
    ($runner:ident($arg:expr)) => {{
        let mut r = $runner.lock().unwrap();
        r($arg.clone())
    }};
    ($runner:ident($arg:expr) as $type:path) => {{
        let mut r = $runner.lock().unwrap();
        let $type(val) = r($arg.clone()) else {
            panic!("invalid ast");
        };
        (**val).clone()
    }};
}

