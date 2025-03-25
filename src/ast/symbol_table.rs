use std::{collections::HashMap, fmt::Display};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolTable<TExpr: Clone> {
    inner: HashMap<Symbol, TExpr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Symbol {
    id: String,
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

impl Symbol {
    pub fn new(id: String) -> Self {
        Self {
            id
        }
    }
}
