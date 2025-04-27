use crate::types::hm::Type;

pub fn int() -> Type {
    Type::Basic("int".to_owned())
}

pub fn float() -> Type {
    Type::Basic("float".to_owned())
}

pub fn string() -> Type {
    Type::Basic("string".to_owned())
}

pub fn color() -> Type {
    Type::Basic("color".to_owned())
}

pub fn array(elem: &Type) -> Type {
    Type::TApp("array".to_owned(), vec![ elem.clone() ])
}

pub fn dict(key: &Type, value: &Type) -> Type {
    Type::TApp("dict".to_owned(), vec![ key.clone(), value.clone() ])
}

pub fn pair(a: &Type, b: &Type) -> Type {
    Type::TApp("pair".to_owned(), vec![ a.clone(), b.clone() ])
}

pub fn func1(arg: &Type, ret: &Type) -> Type {
    Type::Fn(
        Box::new(arg.clone()),
        Box::new(ret.clone())
    )
}

pub fn func2(arg1: &Type, arg2: &Type, ret: &Type) -> Type {
    Type::Fn(
        Box::new(arg1.clone()),
        Box::new(func1(arg2, ret))
    )
}

pub fn func(args: &[Type], ret: &Type) -> Type {
    match args {
        [] => Type::Fn(
            Box::new(Type::TUnit),
            Box::new(ret.clone())
        ),

        [ a ] => Type::Fn(
            Box::new(a.clone()),
            Box::new(ret.clone())
        ),

        [ a, rest @ .. ] => Type::Fn(
            Box::new(a.clone()),
            Box::new(func(rest, ret))
        )
    }
}