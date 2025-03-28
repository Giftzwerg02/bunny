#[macro_export]
macro_rules! library {
    ( $(
         fn $func_name:tt ( $pattern:pat ) $body:block
       )* ) => {{
            let mut map: ::std::collections::HashMap<String, Box<dyn Fn(Vec<Value>) -> Value>> = ::std::collections::HashMap::new();
            $(
                map.insert(
                    stringify!($func_name).to_string(),
                    Box::new(|value: Vec<Value>| -> Value {
                        match value.as_slice() {
                            $pattern => $body,
                            _ => panic!("Pattern match failed for function {}", stringify!($func_name)),
                        }
                    })
                );
            )*
            map
       }};
}