use std::fmt::{Debug, Display};

use text_trees::StringTreeNode;

use super::{Expr, ParsedExpr, AST};

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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SymbolExpr {
    pub value: String,
}

impl Symbol {
    pub fn new(value: String) -> Self {
        Self { value }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum FuncCall<TExpr: Expr> {
    Single(NonEmptyFuncCall<TExpr>),
    Multi(Vec<FuncCall<TExpr>>),
}


impl<TExpr: Expr> FuncCall<TExpr> {
    pub fn pretty_print(&self) -> StringTreeNode {
        match self {
            FuncCall::Single(non_empty_func_call) => {
                let mut childs = vec![StringTreeNode::new("id".to_string())];
                for arg in non_empty_func_call.args.clone() {
                    childs.push(arg.pretty_print());
                }
                StringTreeNode::with_child_nodes("single funccall".to_string(), childs.into_iter())
            }
            FuncCall::Multi(func_calls) => StringTreeNode::with_child_nodes(
                "multi funccall".to_string(),
                func_calls.into_iter().map(|f| f.pretty_print()),
            ),
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct NonEmptyFuncCall<TExpr: Expr> {
    pub id: Symbol<TExpr>,
    pub args: Vec<TExpr>,
}

impl<TExpr: Expr> NonEmptyFuncCall<TExpr> {
    pub fn new(id: TExpr, args: Vec<TExpr>) -> Self {
        Self { id, args }
    }
}

impl <'a> NonEmptyFuncCall<ParsedExpr<'a>> {
    pub fn id(&self) -> Symbol {
        let AST::Symbol(ref s) = *self.id.value else {
            panic!("invalid ast");
        };
        s.clone()
    }

    pub fn args(&self) -> Vec<Argument<ParsedExpr<'a>>> {
        self.args
            .iter()
            .map(|arg| {
                let AST::Argument(ref arg) = *arg.value else {
                    panic!("invalid ast");
                };
                arg.clone()
            })
            .collect()
    }
}

impl<TExpr: Expr> Display for FuncCall<TExpr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuncCall::Single(func) => {
                let args = func
                    .args
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<_>>()
                    .join(" ");

                write!(f, "( {} {args} )", func.id)
            }
            FuncCall::Multi(funcs) => {
                let str = funcs
                    .iter()
                    .map(|f| format!("{f}"))
                    .collect::<Vec<_>>()
                    .join("\n");
                write!(f, "( {str} )")
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Argument<TExpr: Expr> {
    Named(NamedArgument<TExpr>),
    Unnamed(TExpr),
}

impl<TExpr: Expr> Argument<TExpr> {
    pub fn pretty_print(&self) -> StringTreeNode {
        match self {
            Argument::Named(named_argument) => StringTreeNode::with_child_nodes(
                "arg:named".to_string(),
                vec![
                    StringTreeNode::new("id".to_string()),
                    named_argument.expr.pretty_print(),
                ]
                .into_iter(),
            ),
            Argument::Unnamed(expr) => StringTreeNode::with_child_nodes(
                "arg:unnamed".to_string(),
                vec![expr.pretty_print()].into_iter(),
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamedArgument<TExpr: Expr> {
    pub id: TExpr,
    pub expr: TExpr,
}

impl<TExpr: Expr> NamedArgument<TExpr> {
    pub fn new(id: TExpr, expr: TExpr) -> Self {
        Self { id, expr }
    }
}

impl<TExpr: Expr> Display for Argument<TExpr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Argument::Named(named) => write!(f, "{}: {}", named.id, named.expr),
            Argument::Unnamed(expr) => write!(f, "{expr}"),
        }
    }
}
