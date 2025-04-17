mod typed;
mod hm;
mod util;

use crate::ast::scoped::{ScopedStageInfo, SymbolTable};
use crate::ast::{Argument, Array, Color, Dict, DictEntry, Expr, Float, FuncCall, FuncCallSingle, Int, NamedArgument, PositionalArgument, PrettyPrintable, Str, Symbol};
use crate::types::hm::{HMState, Type};
use crate::types::typed::{PolyTypedStageInfo, TypedStageInfo};
use crate::types::util::{array_type, color_type, dict_type, float_type, int_type, pair_type, string_type};

struct InferenceState<'a> {
    type_assumptions: SymbolTable<PolyTypedStageInfo<'a>>,
    hm: HMState
}

fn infer(
    expr: &Expr<ScopedStageInfo>,
    state: &mut InferenceState
) -> Expr<TypedStageInfo> {
    match expr {
        Expr::Int(Int { value, info }) =>
            Expr::Int(
                Int::new(
                    value.clone(),
                    type_stage_info(info, int_type(), state)
                )
            ),

        Expr::Float(Float { value, info }) =>
            Expr::Float(
                Float::new(
                    value.clone(),
                    type_stage_info(info, float_type(), state)
                )
            ),

        Expr::String(Str { value, info }) =>
            Expr::String(
                Str::new(
                    value.clone(),
                    type_stage_info(info, string_type(), state)
                )
            ),

        Expr::Color(Color { r, g, b, info }) =>
            Expr::Color(
                Color::new(
                    *r, *g, *b,
                    type_stage_info(info, color_type(), state)
                )
            ),

        Expr::Symbol(sym) =>
            Expr::Symbol(infer_symbol(sym, state)),

        Expr::FuncCall(FuncCall::Single(call)) =>
            Expr::FuncCall(FuncCall::Single(infer_single_func_call(call, state))),

        Expr::FuncCall(_) =>
            panic!("list-calls should not be present in typechecking stage"),

        Expr::Array(array) =>
            Expr::Array(infer_array(array, state)),

        Expr::Dict(dict) =>
            Expr::Dict(infer_dict(dict, state))
    }
}

fn type_stage_info(
    info: &ScopedStageInfo,
    typ: Type,
    state: &mut InferenceState
) -> TypedStageInfo {
    TypedStageInfo {
        inner: info.inner.clone(),
        typ,
        syms: state.type_assumptions.clone()
    }
}

/// Refer to https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L197
fn infer_symbol(
    sym: &Symbol<ScopedStageInfo>,
    state: &mut InferenceState
) -> Symbol<TypedStageInfo> {
    let key = &sym.value;

    // defs are infered as let-binded vars:
    // https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L241
    if !state.type_assumptions.contains(key) {
        let scoped_expr = sym.info.syms.get(key)
            .expect(&format!("The should be no undefined symbols in the type checking stage: {key}"));

        state.hm.enter_level();

        let expr = infer(scoped_expr, state);
        //let polytype = expr.typ().generalize(&state.hm);
        // TODO: Generalize the whole expr tree, not just the type of expr itself

        state.hm.exit_level();


        state.type_assumptions.insert(key.clone(), expr);
    }

    let polytype = state.type_assumptions.get(key).unwrap();

    polytype.inst(&mut state.hm)
}

/// See: https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L210
fn infer_single_func_call(
    call: &FuncCallSingle<ScopedStageInfo>,
    state: &mut InferenceState
) -> FuncCallSingle<TypedStageInfo> {
    let fn_sym = infer_symbol(&call.id, state);

    // t0 in the paper
    let mut current_typ = fn_sym.info.typ.clone();

    let mut arg_types = Vec::new();

    // TODO We have to sort the function arguments here for this to work with named arguments
    for arg in &call.args {
        // t1 in the paper
        let current_arg_typ = infer_argument(arg, state);
        arg_types.push(current_arg_typ.clone());

        // t' in the paper
        let ret_typ = state.hm.newvar();

        current_typ.unify(
            &Type::Fn(
                Box::new(current_arg_typ.info().typ.clone()),
                Box::new(ret_typ.clone())
            )
        );

        current_typ = ret_typ;
    }

    FuncCallSingle::new(
        fn_sym,
        arg_types,
        type_stage_info(
            &call.info,
            current_typ,
            state
        )
    )
}

fn infer_argument(
    arg: &Argument<ScopedStageInfo>,
    state: &mut InferenceState
) -> Argument<TypedStageInfo> {
    match arg {
        Argument::Positional(PositionalArgument { value, info }) => {
            let expr = infer(value, state);

            Argument::Positional(
                PositionalArgument::new(
                    expr.clone(),
                    expr.info().clone()
                )
            )
        },

        // TODO Checking the name?
        Argument::Named(
            NamedArgument { name: Symbol { value: name, .. },
                value: scoped_expr, .. }
        ) => {
            let expr = infer(scoped_expr, state);

            Argument::Named(
                NamedArgument::new(
                    Symbol::new(name.clone(), expr.info().clone()),
                    expr.clone(),
                    expr.info().clone(),
                )
            )
        }
    }
}

fn infer_array(
    array: &Array<ScopedStageInfo>,
    state: &mut InferenceState
) -> Array<TypedStageInfo> {

    // TODO InteliJ doesn't like this for some reason
    /*if let Some(first) = array.value.first() else {
        return Type::TApp(array_typ_name, vec![ state.hm.newvar() ])
    }*/

    if array.value.is_empty() {
        return Array::new(
            vec![],
            type_stage_info(&array.info, array_type(state.hm.newvar()), state)
        );
    }

    let cloned_values = array.value.clone();
    let first = cloned_values.first().unwrap();
    let element_type = infer(first, state).typ();

    let typed_values = cloned_values
        .into_iter()
        .map(|element| infer(&element, state))
        .collect::<Vec<Expr<TypedStageInfo>>>();

    if typed_values.iter().any(|current| current.typ() != element_type) {
        panic!();
    }

    Array::new(
        typed_values,
        type_stage_info(
            &array.info,
            array_type(element_type.clone()),
            state
        )
    )
}

fn infer_dict(
    dict: &Dict<ScopedStageInfo>,
    state: &mut InferenceState
) -> Dict<TypedStageInfo> {

    if dict.value.is_empty() {
        return Dict::new(
            vec![],
            type_stage_info(
                &dict.info,
                dict_type(state.hm.newvar(), state.hm.newvar()),
                state
            )
        )
    }

    fn infer_dict_entry(
        entry: &DictEntry<ScopedStageInfo>,
        state: &mut InferenceState
    ) -> DictEntry<TypedStageInfo> {
        let key = infer(&entry.key, state);
        let value = infer(&entry.value, state);

        DictEntry::new(
            key.clone(),
            value.clone(),
            type_stage_info(
                &entry.info,
                pair_type(key.typ().clone(), value.typ().clone()),
                state
            )
        )
    }

    let cloned_values = dict.value.clone();

    let first = cloned_values.first().unwrap();
    let entry_expr = infer_dict_entry(first, state);
    let entry_type = entry_expr.info.typ;

    let typed_values = cloned_values
        .iter()
        .map(|entry| infer_dict_entry(entry, state))
        .collect::<Vec<DictEntry<TypedStageInfo>>>();

    let any_type_deviates = typed_values
        .iter()
        .any(|current_entry|
            current_entry.info.typ != entry_type
        );

    if any_type_deviates {
        panic!();
    }

    Dict::new(
        typed_values,
        type_stage_info(
            &dict.info,
            dict_type(
                entry_expr.key.typ().clone(),
                entry_expr.value.typ().clone()
            ),
            state
        )
    )
}