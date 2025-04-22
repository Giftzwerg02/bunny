use crate::types::hm::Type;

pub fn bint() -> Type {
    Type::Basic("int".to_owned())
}

pub fn bfloat() -> Type {
    Type::Basic("float".to_owned())
}

pub fn bstring() -> Type {
    Type::Basic("string".to_owned())
}

pub fn bcolor() -> Type {
    Type::Basic("color".to_owned())
}

pub fn barray(elem: &Type) -> Type {
    Type::TApp("array".to_owned(), vec![ elem.clone() ])
}

pub fn bdict(key: &Type, value: &Type) -> Type {
    Type::TApp("dict".to_owned(), vec![ key.clone(), value.clone() ])
}

pub fn bpair(a: &Type, b: &Type) -> Type {
    Type::TApp("pair".to_owned(), vec![ a.clone(), b.clone() ])
}

pub fn bfunc1(arg: &Type, ret: &Type) -> Type {
    Type::Fn(
        Box::new(arg.clone()),
        Box::new(ret.clone())
    )
}

pub fn bfunc(args: &[Type], ret: &Type) -> Type {
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
            Box::new(bfunc(rest, ret))
        )
    }
}