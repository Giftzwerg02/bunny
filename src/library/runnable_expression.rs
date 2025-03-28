use crate::ast::AST;
use crate::runner::value::Value;
use crate::types::typed_expression::TypedExpr;

#[derive(Debug, Clone, PartialEq)]
pub enum RunnableExpr<'a> {
    Native(NativeExpr),

    Bunny(BunnyExpr<'a>)
}

pub type NativeExpr = Box<dyn Fn(Vec<Value>) -> Value>;

#[derive(Debug, Clone, PartialEq)]
pub struct BunnyExpr<'a> {
    pub value: Box<AST<RunnableExpr<'a>>>,
    pub inner: TypedExpr<'a>
}