use crate::ast::parsed::ParsedStageInfo;
use crate::ast::{Expr, PrettyPrintable, StageInfo};
use std::fmt::{Display, Formatter};
use im::HashMap;
use text_trees::StringTreeNode;
use crate::types::hm::{HMState, PolyType, Type};
use crate::types::InferenceState;

pub type TypedSymbolTable<'a> = HashMap<String, TypedValue<'a>>;

#[derive(Clone, Debug)]
pub enum TypedValue<'a> {
    FromLibrary(PolyType),
    FromBunny(Expr<PolyTypedStageInfo<'a>>)
}

/// This stage info is used for typed expression in the AST itself
#[derive(Clone, Debug)]
pub struct TypedStageInfo<'a> {
    pub inner: ParsedStageInfo<'a>,
    pub typ: Type,
    pub syms: TypedSymbolTable<'a>
}

/// This stage info is used for typed expressions in symbol tables
/// The difference is that this stage info contains PolyTypes, e.g.
/// types which may be "generic"
#[derive(Clone, Debug)]
pub struct PolyTypedStageInfo<'a> {
    pub inner: ParsedStageInfo<'a>,
    pub typ: PolyType,
    pub syms: TypedSymbolTable<'a>
}

impl TypedValue<'_> {
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

impl Display for TypedStageInfo<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "type: {}", self.typ)
    }
}

impl PrettyPrintable for TypedStageInfo<'_> {
    fn pretty_print(&self) -> StringTreeNode {
        StringTreeNode::new(
            format!("type: {}", self.typ)
        )
    }
}

impl<'a> Expr<TypedStageInfo<'a>> {
    pub fn typ(&self) -> &Type {
        &self.info().typ
    }
}

impl<'a> StageInfo for TypedStageInfo<'a> {}

impl<'a> TypedStageInfo<'a> {
    pub fn generalize(self, state: &HMState) -> PolyTypedStageInfo<'a> {
        PolyTypedStageInfo {
            inner: self.inner,
            syms: self.syms,
            typ: self.typ.generalize(state),
        }
    }
}

impl Display for PolyTypedStageInfo<'_> {
    fn fmt(&self, _: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl PrettyPrintable for PolyTypedStageInfo<'_> {
    fn pretty_print(&self) -> StringTreeNode {
        todo!()
    }
}

impl<'a> StageInfo for PolyTypedStageInfo<'a> {}

impl<'a> PolyTypedStageInfo<'a> {
    pub fn inst(self, state: &mut HMState) -> TypedStageInfo<'a> {
        TypedStageInfo {
            inner: self.inner,
            syms: self.syms,
            typ: self.typ.inst(state)
        }
    }
}