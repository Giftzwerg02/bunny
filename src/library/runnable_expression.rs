use crate::runner::value::{Lazy, Value};


pub struct InterpreterSymbolTable {

}

pub enum RunnableExpr<'a> {
    Native(NativeExpr<'a>),

    Bunny(Expr<'a>) // Howwwww
}

pub type NativeExpr<'a> = Box<dyn Fn(Vec<Lazy<'a>>) -> Lazy<'a>>;

