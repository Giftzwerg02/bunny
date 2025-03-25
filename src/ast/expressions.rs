use std::fmt::{Debug, Display};

use super::Expr;

#[derive(Clone, Debug, PartialEq)]
pub struct Color {
    r: u8,
    g: u8,
    b: u8,
}

impl Color {
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
}

impl Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{:02x}{:02x}{:02x}", self.r, self.g, self.b)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Id {
    value: String,
}

impl Id {
    pub fn new(value: String) -> Self {
        Self { value }
    }
}

impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum FuncCall<TExpr: Expr> {
    Empty,
    Call(NonEmptyFuncCall<TExpr>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct NonEmptyFuncCall<TExpr: Clone + Debug + PartialEq> {
    pub id: Id,
    pub args: Vec<Argument<TExpr>>,
}

impl<TExpr: Expr> NonEmptyFuncCall<TExpr> {
    pub fn new(id: Id, args: Vec<Argument<TExpr>>) -> Self {
        Self { id, args }
    }
}

impl <TExpr: Expr> Display for FuncCall<TExpr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuncCall::Empty => write!(f, "()"),
            FuncCall::Call(func) => {
                let str = func.args.iter().map(|arg| format!("{arg}")).collect::<Vec<_>>();
                let str = str.join(" ");
                write!(f, "( {} {str} )", func.id.value)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Argument<TExpr> {
    Named(NamedArgument<TExpr>),
    Unnamed(TExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamedArgument<TExpr> {
    pub id: Id,
    pub expr: TExpr
}

impl<TExpr> NamedArgument<TExpr> {
    pub fn new(id: Id, expr: TExpr) -> Self {
        Self { id, expr }
    }
}

impl <TExpr: Display> Display for Argument<TExpr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Argument::Named(named) => write!(f, "{}: {}", named.id, named.expr),
            Argument::Unnamed(expr) => write!(f, "{expr}"),
        }
    }
}
