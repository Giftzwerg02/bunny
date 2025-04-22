pub mod typed;
mod hm;
pub mod util;

use crate::ast::scoped::{ScopedStageInfo, SymbolValue};
use crate::ast::{Argument, Array, Color, Dict, DictEntry, Expr, Float, FuncCall, FuncCallSingle, Int, Lambda, NamedArgument, Str, Symbol};
use crate::types::hm::{HMState, Type};
use crate::types::typed::{PolyTypedStageInfo, TypedStageInfo, TypedSymbolTable, TypedValue};
use crate::types::util::{array_type, color_type, dict_type, float_type, func_type, int_type, pair_type, string_type};

pub struct InferenceState<'a> {
    pub type_assumptions: TypedSymbolTable<'a>,
    pub hm: HMState
}

impl InferenceState<'_> {
    pub fn new() -> Self {
        InferenceState {
            type_assumptions: TypedSymbolTable::new(),
            hm: HMState::new()
        }
    }
}

pub fn typecheck_pass<'a>(
    expr: &Expr<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Expr<TypedStageInfo<'a>> {
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
            Expr::Dict(infer_dict(dict, state)),

        Expr::Lambda(lambda) =>
            Expr::Lambda(infer_lambda(lambda, state))
    }
}

fn type_stage_info<'a>(
    info: &ScopedStageInfo<'a>,
    typ: Type,
    state: &mut InferenceState<'a>
) -> TypedStageInfo<'a> {
    TypedStageInfo {
        inner: info.inner.clone().expect("to be called only with bunny expressions"),
        typ,
        syms: state.type_assumptions.clone()
    }
}

fn infer_argument_definition<'a>(
    sym: &Symbol<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Symbol<TypedStageInfo<'a>> {
    Symbol::new(
        sym.value.clone(),
        type_stage_info(
            &sym.info,
            state.hm.newvar(),
            state
        )
    )
}

/// Refer to https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L197
fn infer_symbol<'a>(
    sym: &Symbol<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Symbol<TypedStageInfo<'a>> {
    let key = &sym.value;

    // defs are infered as let-binded vars:
    // https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L241
    if !state.type_assumptions.contains_key(key) {
        let scoped_expr = sym.info.syms.get(key)
            .expect(&format!("The should be no undefined symbols in the type checking stage: {key}"));

        state.hm.enter_level();

        let typed_expr = match scoped_expr {
            SymbolValue::Defined =>
                Expr::Symbol(infer_argument_definition(&sym, state)),

            SymbolValue::FunctionDefinition(lambda) =>
                Expr::Lambda(infer_lambda(lambda, state))
        };

        let poly_expr = typed_expr.map_stage(
            &mut |typed_info: TypedStageInfo| typed_info.generalize(&state.hm)
        );

        state.hm.exit_level();

        state.type_assumptions.insert(key.clone(), TypedValue::FromBunny(poly_expr));
    }

    let symbol: &TypedValue = state.type_assumptions
        .get(key)
        .unwrap();

    let symbol_type = symbol.inst(&mut state.hm);

    Symbol::new(
        sym.value.clone(),
        type_stage_info(&sym.info, symbol_type, state)
    )
}

/// See: https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L210
fn infer_single_func_call<'a>(
    call: &FuncCallSingle<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> FuncCallSingle<TypedStageInfo<'a>> {
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

fn infer_argument<'a>(
    arg: &Argument<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Argument<TypedStageInfo<'a>> {
    match arg {
        Argument::Positional(scoped_expr) => {
            let typed_expr = typecheck_pass(scoped_expr, state);

            Argument::Positional(typed_expr)
        },

        // TODO Checking the name?
        Argument::Named(
            NamedArgument { name: Symbol { value: name, .. },
                value: scoped_expr, .. }
        ) => {
            let expr = typecheck_pass(scoped_expr, state);

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

fn infer_array<'a>(
    array: &Array<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Array<TypedStageInfo<'a>> {

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

    let elem = typecheck_pass(first, state);

    let typed_values = cloned_values
        .into_iter()
        .map(|element| typecheck_pass(&element, state))
        .collect::<Vec<Expr<TypedStageInfo>>>();

    if typed_values.iter().any(|current| current.typ() != elem.typ()) {
        panic!();
    }

    Array::new(
        typed_values,
        type_stage_info(
            &array.info,
            array_type(elem.typ().clone()),
            state
        )
    )
}

fn infer_dict<'a>(
    dict: &Dict<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Dict<TypedStageInfo<'a>> {

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

    fn infer_dict_entry<'a>(
        entry: &DictEntry<ScopedStageInfo<'a>>,
        state: &mut InferenceState<'a>
    ) -> DictEntry<TypedStageInfo<'a>> {
        let key = typecheck_pass(&entry.key, state);
        let value = typecheck_pass(&entry.value, state);

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

fn infer_lambda<'a>(
    lambda: &Lambda<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Lambda<TypedStageInfo<'a>> {
    let typed_body = typecheck_pass(&lambda.body, state);

    let typed_symbols = &typed_body.info().syms;

    let typed_args = lambda
        .clone()
        .args
        .into_iter()
        .map(|Symbol { value, info }| {
            let typed_expr = typed_symbols.get(&value)
                .expect("Argument value to be typed in symbol table of body");

            let typ = typed_expr.inst(&mut state.hm);

            Symbol::new(value, type_stage_info(&info, typ, state))
        })
        .collect::<Vec<Symbol<TypedStageInfo>>>();

    let arg_types = typed_args
        .iter()
        .map(|Symbol { info, .. }| info.typ.clone())
        .collect();

    let fun_type = func_type(arg_types, typed_body.typ().clone());

    Lambda::parametric(
        typed_args,

        typed_body,

        type_stage_info(&lambda.info, fun_type, state)
    )
}

/*
#[cfg(test)]
mod tests {
    use crate::ast;
    use crate::ast::{Expr, Lambda, StageInfo};
    use crate::ast::parsed::{parsed_expr_pass, ParsedStageInfo};
    use crate::ast::scoped::{scoped_expr_pass, ScopedStageInfo, SymbolTable, SymbolValue};
    use crate::parser::{BunnyParser, Rule};
    use crate::types::hm::Type;

    fn assert_type(expr: &'static str, typ: Type) {
        fn empty_func<I: StageInfo>(info: I) -> ast::FuncCallSingle<I> {
            ast::FuncCallSingle::new(
                ast::Symbol::new("".to_owned(), info.clone()),
                vec![],
                info.clone(),
            )
        }

        fn empty_func_expr<I: StageInfo>(info: I) -> Expr<I> {
            Expr::FuncCall(ast::FuncCall::Single(empty_func(info)))
        }

        let pair = BunnyParser::parse(Rule::program, code)
            .unwrap()
            .next()
            .unwrap();

        let mut syms = SymbolTable::new();
        let info = ScopedStageInfo::new(ParsedStageInfo::new(pair.clone()), syms.clone());

        syms.insert("def".to_string(), SymbolValue::Defined);
        syms.insert(r"\".to_string(), SymbolValue::Defined);
        let empty = Lambda::constant(crate::ast::scoped::tests::empty_func_expr(info.clone()), info);
        syms.insert("+".to_string(), empty.into());

        let parsed_expr = parsed_expr_pass(pair);
        scoped_expr_pass(parsed_expr, &syms);
    }

    fn assert_panics(expr: &'static) {

    }
}*/