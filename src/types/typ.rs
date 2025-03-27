use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Color,
    Opaque,

    IntArray,
    FloatArray,
    StringArray,
    ColorArray,
    OpaqueArray,

    IntDict,
    FloatDict,
    StringDict,
    ColorDict,
    OpaqueDict
}


impl Type {
    pub fn is_dict(&self) -> bool {
        matches!(
            self,
            Type::IntDict | Type::FloatDict | Type::StringDict | Type::ColorDict | Type::OpaqueDict
        )
    }

    pub fn is_array(&self) -> bool {
        matches!(
            self,
            Type::IntArray | Type::FloatArray | Type::StringArray | Type::ColorArray | Type::OpaqueArray
        )
    }

    pub fn to_array_type(&self) -> Type {
        match self {
            Type::Int => Type::IntArray,
            Type::Float => Type::FloatArray,
            Type::String => Type::StringArray,
            Type::Color => Type::ColorArray,
            Type::Opaque => Type::OpaqueArray,
            _ => panic!("Tried to convert {:?} to array type", self),
        }
    }

    pub fn to_dict_type(&self) -> Type {
        match self {
            Type::Int => Type::IntDict,
            Type::Float => Type::FloatDict,
            Type::String => Type::StringDict,
            Type::Color => Type::ColorDict,
            Type::Opaque => Type::OpaqueDict,
            _ => panic!("Tried to convert {:?} to dict type", self),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let val = match self {
            Type::Int         => "int",
            Type::Float       => "float",
            Type::String      => "string",
            Type::Color       => "color",
            Type::Opaque      => "opaque",
            Type::IntArray    => "array[int]",
            Type::FloatArray  => "array[float]",
            Type::StringArray => "array[string]",
            Type::ColorArray  => "array[color]",
            Type::OpaqueArray => "array[opaque]",
            Type::IntDict     => "dict[int]",
            Type::FloatDict   => "dict[float]",
            Type::StringDict  => "dict[string]",
            Type::ColorDict   => "dict[color]",
            Type::OpaqueDict  => "dict[opaque]",
        };

        write!(f, "{}", val)
    }
}
