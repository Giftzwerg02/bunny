use std::fmt::{Display, Formatter};
use text_trees::StringTreeNode;
use crate::ast::{Expr, ScopedExpr, AST};
use crate::types::typ::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpr<'a> {
    pub(crate) value: Box<AST<TypedExpr<'a>>>,
    pub(crate) inner: ScopedExpr<'a>,
    pub(crate) typ: Type
}
impl Display for TypedExpr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}) {}", self.typ, self.inner)
    }
}

impl Expr for TypedExpr<'_> {
    fn pretty_print(&self) -> StringTreeNode {
        let parsed_expr = self.inner.pretty_print();
        let data = format!("({}) {}", self.typ, parsed_expr.data());

        match &*self.value {
            AST::Int(_) | AST::Float(_) | AST::String(_) |
            AST::Color(_color) | AST::Id(_id) => StringTreeNode::new(data),

            AST::FuncCall(func_call) => func_call.pretty_print(),
            AST::Argument(argument) => argument.pretty_print(),
            AST::Array(items) => StringTreeNode::with_child_nodes(
                data,
                items.into_iter().map(|i| i.pretty_print())
            ),
            AST::Dict(items) => StringTreeNode::with_child_nodes(
                data,
                items.into_iter().map(|i| {
                    StringTreeNode::with_child_nodes(
                        "entry".to_string(),
                        vec![i.0.pretty_print(), i.1.pretty_print()].into_iter()
                    )
                })
            ),
        }
    }
}