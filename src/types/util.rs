use crate::types::hm::Type;

pub fn int_type() -> Type {
    Type::Basic("int".to_owned())
}

pub fn float_type() -> Type {
    Type::Basic("float".to_owned())
}

pub fn string_type() -> Type {
    Type::Basic("string".to_owned())
}

pub fn color_type() -> Type {
    Type::Basic("color".to_owned())
}

pub fn array_type(elem: Type) -> Type {
    Type::TApp("array".to_owned(), vec![ elem ])
}

pub fn dict_type(key: Type, value: Type) -> Type {
    Type::TApp("dict".to_owned(), vec![ key, value ])
}

pub fn pair_type(a: Type, b: Type) -> Type {
    Type::TApp("pair".to_owned(), vec![ a, b ])
}

pub fn func_type(args: Vec<Type>, ret: Type) -> Type {
    match &args[..] {
        [] => Type::Fn(
            Box::new(Type::TUnit),
            Box::new(ret)
        ),

        [ a ] => Type::Fn(
            Box::new(a.clone()),
            Box::new(ret)
        ),

        [ a, rest @ .. ] => Type::Fn(
            Box::new(a.clone()),
            Box::new(func_type(rest.to_vec(), ret))
        )
    }
}