pub mod typ;
pub mod typed_expression;

use crate::ast::{Expr, ScopedExpr, AST};
use std::fmt::Display;
use proptest::char::range;
use crate::ast::expressions::{Argument, Color, FuncCall, NamedArgument, Symbol};
use crate::ast::symbol_table::SymbolTable;
use crate::types::typ::Type;
use crate::types::typed_expression::TypedExpr;

pub fn typecheck(parsed_expr: ScopedExpr, symbols: &SymbolTable<TypedExpr>) -> TypedExpr {
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
    parsed_expr: ScopedExpr,
    symbols: &SymbolTable<TypedExpr>
) -> TypedExpr {

}

fn typecheck_argument(
    call: Argument<ScopedExpr>,
    parsed_expr: ScopedExpr,
    symbols: &SymbolTable<TypedExpr>
) -> TypedExpr {
    let (value, typ) = match call {
        Argument::Named(NamedArgument { id, expr }) => {
            let typed_expr = typecheck(expr, &symbols);

            (
                AST::Argument(
                    Argument::Named(NamedArgument::new(id, typed_expr.clone()))),
                typed_expr.typ
            )
        }

        Argument::Unnamed(expr) => {
            let typed_expr = typecheck(expr, &symbols);

            (
                AST::Argument(Argument::Unnamed(typed_expr.clone())),
                typed_expr.typ
            )
        }
    };

    TypedExpr {
        inner: parsed_expr,
        value: Box::new(value),
        typ
    }
}

fn typecheck_func_call(
    call: FuncCall<ScopedExpr>,
    parsed_expr: ScopedExpr,
    symbols: &SymbolTable<TypedExpr>
) -> TypedExpr {

}

fn typecheck_array(
    array: Vec<ScopedExpr>,
    parsed_expr: ScopedExpr,
    symbols: &SymbolTable<TypedExpr>
) -> TypedExpr {
    if(array.is_empty()){
        panic!("Cannot infer type of empty array");
    }

    let array_elem_type = typecheck(array[0].clone(), &symbols).typ;

    let typed_elems: Vec<TypedExpr> = array
        .into_iter()
        .map(|elem| typecheck(elem.clone(), &symbols))
        .collect();

    let invalid_elem = typed_elems
        .iter()
        .find(|elem| elem.typ == array_elem_type);

    if let Ok(elem) = invalid_elem {
        panic!("{} element in {:?}", elem, typed_elems);
    }

    TypedExpr {
        value: Box::new(AST::Array(typed_elems)),
        inner: parsed_expr,
        typ: array_elem_type.to_array_type()
    }
}

fn typecheck_dict(
    dict: Vec<(ScopedExpr, ScopedExpr)>,
    parsed_expr: ScopedExpr,
    symbols: &SymbolTable<TypedExpr>
) -> TypedExpr {
    if(dict.is_empty()){
        panic!("Cannot infer type of empty dictionary");
    }

    let (_, value) = dict.first().unwrap();
    let dict_val_type = typecheck(value.clone(), &symbols).typ;

    let typed_entries: Vec<(TypedExpr, TypedExpr)> = dict.into_iter()
        .map(|(key, value)| {
            let typed_key = typecheck(key.clone(), &symbols);
            let typed_val = typecheck(value.clone(), &symbols);

            if(typed_key.typ != Type::String){
                panic!("Map with {} keys not allowed", typed_key.typ);
            }

            if(typed_val.typ != dict_val_type){
                panic!("Element of typ {} in map of type {}", typed_val.typ, dict_val_type)
            }

            (typed_key, typed_val)
        })
        .collect();

    TypedExpr {
        value: Box::new(AST::Dict(typed_entries)),
        inner: parsed_expr,
        typ: dict_val_type.to_dict_type()
    }
}

fn create_int(value: u64, parsed_expr: ScopedExpr) -> TypedExpr {
    TypedExpr {
        value: Box::new(AST::Int(value)),
        inner: parsed_expr,
        typ: Type::Int
    }
}


pub fn create_float(value: f64, parsed_expr: ScopedExpr) -> TypedExpr {
    TypedExpr {
        value: Box::new(AST::Float(value)),
        inner: parsed_expr,
        typ: Type::Float,
    }
}

pub fn create_string(value: String, parsed_expr: ScopedExpr) -> TypedExpr {
    TypedExpr {
        value: Box::new(AST::String(value)),
        inner: parsed_expr,
        typ: Type::String,
    }
}

pub fn create_color(value: Color, parsed_expr: ScopedExpr) -> TypedExpr {
    TypedExpr {
        value: Box::new(AST::Color(value)),
        inner: parsed_expr,
        typ: Type::Color,
    }
}