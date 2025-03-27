use std::{collections::HashMap, fmt::Display};
use crate::ast::expressions::Symbol;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolTable<TExpr: Clone> {
    inner: HashMap<Symbol, TExpr>,
}

impl <TExpr: Display + Clone> SymbolTable<TExpr> {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn insert(&mut self, sym: Symbol, value: TExpr) -> bool {
        self.inner.insert(sym, value).is_some()
    }

    pub fn contains(&self, sym: &Symbol) -> bool {
        self.inner.contains_key(sym)
    }
}