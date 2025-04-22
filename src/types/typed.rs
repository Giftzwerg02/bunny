use crate::ast::parsed::ParsedStageInfo;
use crate::ast::{Expr, PrettyPrintable, StageInfo};
use std::fmt::{Display, Formatter};
use im::HashMap;
use text_trees::StringTreeNode;
use crate::types::hm::{HMState, PolyType, Type};

pub type TypedSymbolTable<T> = HashMap<String, Expr<T>>;

#[derive(Clone, Debug)]
pub struct TypedStageInfo<'a> {
    pub inner: ParsedStageInfo<'a>,
    pub typ: Type,
    pub syms: TypedSymbolTable<PolyTypedStageInfo<'a>>
}

#[derive(Clone, Debug)]
pub struct PolyTypedStageInfo<'a> {
    pub inner: ParsedStageInfo<'a>,
    pub typ: PolyType,
    pub syms: TypedSymbolTable<PolyTypedStageInfo<'a>>
}

impl Display for TypedStageInfo<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl PrettyPrintable for TypedStageInfo<'_> {
    fn pretty_print(&self) -> StringTreeNode {
        todo!()
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