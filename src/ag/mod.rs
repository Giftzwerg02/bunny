// use std::collections::HashSet;
//
// #[derive(Debug)]
// pub struct Program {
//     inner: Vec<FuncCall> // i love lisp
// }
//
// #[derive(Debug)]
// pub struct FuncCall {
//     id: String,
//     args: Vec<Argument>,
//     body: Expr,
//     syms: SymbolTable,
// }
//
// #[derive(Debug)]
// pub enum Argument {
//     Named(NamedArgument),
//     Expr(ExprArgument),
// }
//
// #[derive(Debug)]
// pub struct NamedArgument {
//     id: String,
//     value: Expr,
// }
//
// #[derive(Debug)]
// pub struct ExprArgument {
//     value: Expr,
// }
//
// #[derive(Debug)]
// pub struct Expr {
//     value: ExprValue,
//     syms: SymbolTable,
// }
//
// #[derive(Debug)]
// pub enum ExprValue {
//     Int(Int),
//     Float(Float),
//     String(String),
//     Array(Array),
//     Map(Map),
//     FuncCall(FuncCall),
//     Color(Color),
//     Id(Id)
// }
//
// pub type Int = i64;
// pub type Float = f64;
// pub type String = String;
//
// #[derive(Debug)]
// pub struct Id {
//     value: String
// }
//
// #[derive(Debug)]
// pub enum Array {
//     IntArray(Vec<Int>),
//     FloatArray(Vec<Float>),
//     ArrayArray(Vec<Array>),
//     IntArray(Vec<i64>),
//     IntArray(Vec<i64>),
// }
//
// #[derive(Debug)]
// pub enum Map {
//
// }
//
// #[derive(Debug)]
// pub struct Color {
//     r: u8,
//     g: u8,
//     b: u8,
// }
//
//
// #[derive(Debug)]
// pub struct SymbolTable {
//     // maybe using a vec is more efficient (?) given a program won't have 1mil entries here...
//     inner: HashSet<Symbol>,
// }
//
// #[derive(Debug, PartialEq, Eq, Hash)]
// pub struct Symbol {
//     id: String,
// }
//
// impl SymbolTable {
//     pub fn new() -> Self {
//         Self {
//             inner: HashSet::new(),
//         }
//     }
//
//     pub fn insert(&mut self, sym: Symbol) -> bool {
//         if !self.inner.insert(sym) {
//             panic!("shadowing not implemented")
//         }
//     }
//
//     pub fn contains(&self, sym: &Symbol) -> bool {
//         self.inner.contains(sym)
//     }
// }
//
// impl Symbol {
//     pub fn new(id: String) -> Self {
//         Self {
//             id
//         }
//     }
// }
