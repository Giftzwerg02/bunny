use crate::ast::parsed::ParsedStageInfo;
use crate::ast::{Expr, PrettyPrintable, StageInfo};
use std::fmt::{Display, Formatter};
use im::HashMap;
use text_trees::StringTreeNode;
use crate::types::hm::{HMState, PolyType, Type};

pub type TypedSymbolTable = HashMap<String, TypedValue>;

#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug)]
pub enum TypedValue {
    FromLibrary(PolyType),
    FromBunny(Expr<PolyTypedStageInfo>)
}

/// This stage info is used for typed expression in the AST itself
#[derive(Clone, Debug)]
pub struct TypedStageInfo {
    pub inner: ParsedStageInfo,
    pub typ: Type,
    pub syms: TypedSymbolTable
}

/// This stage info is used for typed expressions in symbol tables
/// The difference is that this stage info contains PolyTypes, e.g.
/// types which may be "generic"
#[derive(Clone, Debug)]
pub struct PolyTypedStageInfo {
    pub inner: ParsedStageInfo,
    pub typ: PolyType,
    pub syms: TypedSymbolTable
}

impl TypedValue {
    pub fn inst(&self, state: &mut HMState) -> Type {
        match self {
            TypedValue::FromLibrary(polytype) =>
                polytype.inst(state),

            TypedValue::FromBunny(expr) =>
                expr
                    .clone()
                    .map_stage(&mut |poly_typed_info: PolyTypedStageInfo| poly_typed_info.inst(state))
                    .typ()
                    .clone()
        }
    }
}

impl Display for TypedStageInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "type: {}", self.typ)
    }
}

impl PrettyPrintable for TypedStageInfo {
    fn pretty_print(&self) -> StringTreeNode {
        StringTreeNode::new(
            format!("type: {}", self.typ)
        )
    }
}

impl Expr<TypedStageInfo> {
    pub fn typ(&self) -> &Type {
        &self.info().typ
    }
}

impl StageInfo for TypedStageInfo {}

impl TypedStageInfo {
    pub fn generalize(self, state: &HMState) -> PolyTypedStageInfo {
        PolyTypedStageInfo {
            inner: self.inner,
            syms: self.syms,
            typ: self.typ.generalize(state),
        }
    }
}

impl Display for PolyTypedStageInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "poly type: {}", self.typ.typ)
    }
}

impl PrettyPrintable for PolyTypedStageInfo {
    fn pretty_print(&self) -> StringTreeNode {
        StringTreeNode::new(format!("poly type: {}", self.typ.typ))
    }
}

impl StageInfo for PolyTypedStageInfo {}

impl PolyTypedStageInfo {
    pub fn inst(self, state: &mut HMState) -> TypedStageInfo {
        TypedStageInfo {
            inner: self.inner,
            syms: self.syms,
            typ: self.typ.inst(state)
        }
    }
}

#[derive(Clone, Debug)]
pub struct AnyTypedStageInfo {
    pub syms: TypedSymbolTable
}

impl StageInfo for AnyTypedStageInfo {}

impl PrettyPrintable for AnyTypedStageInfo {
    fn pretty_print(&self) -> StringTreeNode {
        StringTreeNode::new("any stage info".to_owned())
    }
}

impl Display for AnyTypedStageInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "any stage info")
    }
}

impl From<PolyTypedStageInfo> for AnyTypedStageInfo {
    fn from(poly_typed: PolyTypedStageInfo) -> Self {
        Self {
            syms: poly_typed.syms
        }
    }
}

impl From<TypedStageInfo> for AnyTypedStageInfo {
    fn from(typed: TypedStageInfo) -> Self {
        Self {
            syms: typed.syms
        }
    }
}
