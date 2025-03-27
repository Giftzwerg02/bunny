mod typ;
mod typed_expression;

use crate::ast::{Expr, ParsedExpr, AST};
use std::fmt::Display;
use crate::ast::expressions::{Argument, Color, FuncCall, Symbol};
use crate::ast::symbol_table::SymbolTable;
use crate::types::typ::Type;
use crate::types::typed_expression::TypedExpr;

pub fn typecheck(parsed_expr: ParsedExpr, symbols: SymbolTable<TypedExpr>) -> TypedExpr {
    match &*parsed_expr.value {
        AST::Int(value) =>
            create_int(*value, parsed_expr),

        AST::Float(value) =>
            create_float(*value, parsed_expr),

        AST::String(value) =>
            create_string(value.clone(), parsed_expr),

        AST::Color(value) =>
            create_color(value.clone(), parsed_expr),

        AST::Id(id) =>
            typecheck_id(id.clone(), parsed_expr, symbols),

        AST::FuncCall(call) =>
            typecheck_func_call(call.clone(), parsed_expr, symbols),

        AST::Argument(argument) =>
            typecheck_argument(argument.clone(), parsed_expr, symbols),

        AST::Array(array) =>
            typecheck_array(array.clone(), parsed_expr, symbols),

        AST::Dict(dict) =>
            typecheck_dict(dict.clone(), parsed_expr, symbols),
    }
}

fn typecheck_id(
    id: Symbol,
    parsed_expr: ParsedExpr,
    symbols: SymbolTable<TypedExpr>
) -> TypedExpr {

}

fn typecheck_argument(
    call: Argument<ParsedExpr>,
    parsed_expr: ParsedExpr,
    symbols: SymbolTable<TypedExpr>
) -> TypedExpr {

}

fn typecheck_func_call(
    argument: FuncCall<ParsedExpr>,
    parsed_expr: ParsedExpr,
    symbols: SymbolTable<TypedExpr>
) -> TypedExpr {

}

fn typecheck_array(
    array: Vec<ParsedExpr>,
    parsed_expr: ParsedExpr,
    symbols: SymbolTable<TypedExpr>
) -> TypedExpr {
    if(array.is_empty()){
        panic!("Cannot infer type of empty array");
    }
}

fn typecheck_dict(
    dict: Vec<(ParsedExpr, ParsedExpr)>,
    parsed_expr: ParsedExpr,
    symbols: SymbolTable<TypedExpr>
) -> TypedExpr {

}

fn create_int(value: u64, parsed_expr: ParsedExpr) -> TypedExpr {
    TypedExpr {
        value: Box::new(AST::Int(value)),
        expr: parsed_expr,
        typ: Type::Int
    }
}


pub fn create_float(value: f64, parsed_expr: ParsedExpr) -> TypedExpr {
    TypedExpr {
        value: Box::new(AST::Float(value)),
        expr: parsed_expr,
        typ: Type::Float,
    }
}

pub fn create_string(value: String, parsed_expr: ParsedExpr) -> TypedExpr {
    TypedExpr {
        value: Box::new(AST::String(value)),
        expr: parsed_expr,
        typ: Type::String,
    }
}

pub fn create_color(value: Color, parsed_expr: ParsedExpr) -> TypedExpr {
    TypedExpr {
        value: Box::new(AST::Color(value)),
        expr: parsed_expr,
        typ: Type::Color,
    }
}