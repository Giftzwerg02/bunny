mod typed;
mod hm;

use im::HashMap;
use crate::ast::{Argument, Array, Dict, DictEntry, Expr, FuncCall, FuncCallSingle, Symbol};
use crate::ast::scoped::{ScopedStageInfo, SymbolTable};
use crate::types::hm::{InferenceState, PolyType, Type};
use crate::types::typed::TypedStageInfo;

pub fn typecheck_pass<'a>(
    expr: Expr<ScopedStageInfo>,
    symbol_table: SymbolTable<TypedStageInfo>
) -> Expr<TypedStageInfo> {
    let mut inference = InferenceState::new();
    let type_assumtpions = HashMap::new();

    // TODO This is inefficient as hell, isn't it? It visits all nodes n^2 times
    expr.map_info::<
        dyn Fn(Expr<ScopedStageInfo>, ScopedStageInfo) -> TypedStageInfo, TypedStageInfo
    >(|expr, info| {
        let typ = infer(&expr, type_assumtpions, &mut inference);

        TypedStageInfo {
            inner: info.inner,
            typ,
            syms: symbol_table // TODO Type assumptions go here somehow
        }
    })
}

fn infer(
    expr: &Expr<ScopedStageInfo>,
    type_assumptions: HashMap<String, PolyType>,
    inference: &mut InferenceState
) -> Type {
    match expr {
        Expr::Int(_) =>
            Type::Basic("int".to_owned()),

        Expr::Float(_) =>
            Type::Basic("float".to_owned()),

        Expr::String(_) =>
            Type::Basic("string".to_owned()),

        Expr::Color(_) =>
            Type::Basic("color".to_owned()),

        Expr::Symbol(sym) =>
            infer_symbol(sym, type_assumptions, inference),

        Expr::FuncCall(FuncCall::Single(call)) =>
            infer_func_call(call, type_assumptions, inference),

        Expr::FuncCall(_) => panic!("Func Call Lists should not exist at typechecking stage"),

        Expr::Argument(arg) =>
            infer_argument(arg, type_assumptions, inference),

        Expr::Array(array) =>
            infer_array(array, type_assumptions, inference),

        Expr::Dict(dict) =>
            infer_dict(dict, type_assumptions, inference)
    }
}

/// Refer to https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L197
fn infer_symbol(
    sym: &Symbol<ScopedStageInfo>,
    type_assumptions: HashMap<String, PolyType>,
    inference: &mut InferenceState
) -> Type {
    let key = &sym.value;

    // defs are infered as let-binded vars:
    // https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L241
    let type_assumptions = if !type_assumptions.contains_key(key) {
        inference.enter_level();

        let scoped_expr = sym.info.syms.get(key)
            .expect("The should be no undefined symbols in the type checking stage");

        let typ = infer(scoped_expr, type_assumptions.clone(), inference);
        let polytype = typ.generalize(inference);

        inference.exit_level();

        type_assumptions.update(key.clone(), polytype)
    }
    else {
        type_assumptions
    };

    let polytype = type_assumptions.get(key).unwrap();

    polytype.inst(inference)
}

/// See: https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L210
fn infer_func_call(
    call: &FuncCallSingle<ScopedStageInfo>,
    type_assumptions: HashMap<String, PolyType>,
    inference: &mut InferenceState
) -> Type {
    let fn_typ = infer_symbol(&call.id, type_assumptions.clone(), inference);

    // t0 in the paper
    let mut current_typ = fn_typ;

    // TODO We have to sort the function arguments here for this to work with named arguments
    for arg in call.args {
        // t1 in the paper
        let current_arg_typ = infer_argument(&arg, type_assumptions.clone(), inference);
        // t' in the paper
        let ret_typ = inference.newvar();

        current_typ.unify(
            &Type::Fn(
                Box::new(current_arg_typ),
                Box::new(ret_typ.clone())
            )
        );

        current_typ = ret_typ;
    }

    current_typ
}

fn infer_argument(
    arg: &Argument<ScopedStageInfo>,
    type_assumptions: HashMap<String, PolyType>,
    inference: &mut InferenceState
) -> Type {
    match arg {
        Argument::Positional(positional_arg) =>
            infer(&*positional_arg.value, type_assumptions, inference),

        // TODO Checking the name?
        Argument::Named(named_arg) =>
            infer(&*named_arg.value, type_assumptions, inference),
    }
}

fn infer_array(
    array: &Array<ScopedStageInfo>,
    type_assumptions: HashMap<String, PolyType>,
    inference: &mut InferenceState
) -> Type {
    let array_type_name = "array".to_owned();

    // TODO InteliJ doesn't like this for some reason
    /*if let Some(first) = array.value.first() else {
        return Type::TApp(array_typ_name, vec![ inference.newvar() ])
    }*/

    if array.value.is_empty() {
        return Type::TApp(array_type_name, vec![ inference.newvar() ])
    }

    let first = array.value.first().unwrap();
    let element_type = infer(first, type_assumptions.clone(), inference);

    for element in &array.value {
        let current_element_type = infer(element, type_assumptions.clone(), inference);

        if element_type != current_element_type {
            panic!()
        }
    }

    Type::TApp(array_type_name, vec![ element_type ])
}

fn infer_dict(
    dict: &Dict<ScopedStageInfo>,
    type_assumptions: HashMap<String, PolyType>,
    inference: &mut InferenceState
) -> Type {
    let dict_type_name = "dict".to_owned();

    if dict.value.is_empty() {
        return Type::TApp(dict_type_name, vec![ inference.newvar(), inference.newvar() ])
    }

    fn infer_dict_entry(
        entry: &DictEntry<ScopedStageInfo>,
        type_assumptions: HashMap<String, PolyType>,
        inference: &mut InferenceState
    ) -> (Type, Type) {
        (
            infer(&entry.key, type_assumptions.clone(), inference),
            infer(&entry.value, type_assumptions, inference)
        )
    }

    let first = dict.value.first().unwrap();
    let element_type = infer_dict_entry(first, type_assumptions.clone(), inference);

    for entry in &dict.value {
        let current_entry_type = infer_dict_entry(entry, type_assumptions.clone(), inference);

        if element_type != current_entry_type {
            panic!()
        }
    }

    let (key_type, value_type) = element_type;
    Type::TApp(dict_type_name, vec![ key_type, value_type ])
}