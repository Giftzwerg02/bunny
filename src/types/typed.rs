use crate::ast::parsed::ParsedStageInfo;
use crate::ast::scoped::SymbolTable;
use crate::ast::{Expr, PrettyPrintable, StageInfo};
use std::fmt::{Display, Formatter};
use text_trees::StringTreeNode;
use crate::types::hm::{PolyType, Type};

#[derive(Clone, Debug)]
pub struct TypedStageInfo<'a> {
    pub inner: ParsedStageInfo<'a>,
    pub typ: Type,
    pub syms: SymbolTable<PolyTypedStageInfo<'a>>
}

#[derive(Clone, Debug)]
pub struct PolyTypedStageInfo<'a> {
    pub inner: ParsedStageInfo<'a>,
    pub typ: PolyType,
    pub syms: SymbolTable<PolyTypedStageInfo<'a>>
}

impl<'a> StageInfo for TypedStageInfo<'a> {}

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

impl Expr<TypedStageInfo> {
    pub fn typ(&self) -> &Type {
        &self.info().typ
    }
}