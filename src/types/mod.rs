pub mod typed;
mod hm;
pub mod util;

use hm::HMError;
use miette::{Diagnostic, SourceSpan, Result};
use thiserror::Error;

use crate::ast::parsed::ParsedStageInfo;
use crate::ast::scoped::{ScopedStageInfo, SymbolValue};
use crate::ast::{Argument, Array, Color, Dict, DictEntry, Expr, Float, FuncCall, FuncCallSingle, Int, Lambda, NamedArgument, Str, Symbol};
use crate::types::hm::{HMState, Type};
use crate::types::typed::{TypedStageInfo, TypedSymbolTable, TypedValue};
use crate::types::util::{array, color, dict, float, func, int, pair, string};

#[derive(Clone)]
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

#[derive(Error, Debug, Diagnostic)]
#[error("type error")]
#[diagnostic()]
struct TypeError {
    #[label("here")]
    token: Option<SourceSpan>,

    #[help]
    advice: String
}

pub fn typecheck_pass<'a>(
    expr: &Expr<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Result<Expr<TypedStageInfo<'a>>> {
    let new_expr = match expr {
        Expr::Int(Int { value, info }) =>
            Expr::Int(
                Int::new(
                    value.clone(),
                    type_stage_info(info, int(), state)
                )
            ),

        Expr::Float(Float { value, info }) =>
            Expr::Float(
                Float::new(
                    value.clone(),
                    type_stage_info(info, float(), state)
                )
            ),

        Expr::String(Str { value, info }) =>
            Expr::String(
                Str::new(
                    value.clone(),
                    type_stage_info(info, string(), state)
                )
            ),

        Expr::Color(Color { r, g, b, info }) =>
            Expr::Color(
                Color::new(
                    *r, *g, *b,
                    type_stage_info(info, color(), state)
                )
            ),

        Expr::Symbol(sym) =>
            Expr::Symbol(infer_symbol(sym, state)?),

        Expr::FuncCall(FuncCall::Single(call)) =>
            Expr::FuncCall(FuncCall::Single(infer_single_func_call(call, state)?)),

        Expr::FuncCall(_) =>
            panic!("list-calls should not be present in typechecking stage"),

        Expr::Array(array) =>
            Expr::Array(infer_array(array, state)?),

        Expr::Dict(dict) =>
            Expr::Dict(infer_dict(dict, state)?),

        Expr::Lambda(lambda) =>
            Expr::Lambda(infer_lambda(lambda, state)?)
    };

    Ok(new_expr)
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

fn create_argument_definition<'a>(
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

fn unification_error<T>(
    hmerror: HMError,
    expected: &Type,
    actual: &Type,
    source: &Option<ParsedStageInfo>
) -> Result<T> {
    let token = source.clone().map(|source| {
        let span = source.token.as_span();
        (span.start(), span.end()).into()
    });

    let error = match hmerror {
        HMError::UnificationError(_, _) => 
            TypeError {
                token,
                advice: format!("Types {expected} and {actual} are not compatible")
            },

        HMError::OccursCheckError(typ) => 
            TypeError {
                token,
                advice: format!("Creating an infinite type with {typ} is not allowed")
            }
    };

    Err(error.into())
}

/// Refer to https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L197
fn infer_symbol<'a>(
    sym: &Symbol<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Result<Symbol<TypedStageInfo<'a>>> {
    let key = &sym.value;

    // defs are infered as let-binded vars:
    // https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L241
    if !state.type_assumptions.contains_key(key) {
        let scoped_expr = sym.info.syms.get(key)
            .expect(&format!("The should be no undefined symbols in the type checking stage: {key}"));

        state.hm.enter_level();

        let typed_expr = match scoped_expr {
            SymbolValue::Argument(Argument::Positional(_)) |
            SymbolValue::Defined =>
                // TODO Uhhh, when is this used now?
                Expr::Symbol(create_argument_definition(&sym, state)),

            SymbolValue::FunctionDefinition(lambda) =>
                Expr::Lambda(infer_lambda(lambda, &mut state.clone())?),

            SymbolValue::Argument(Argument::Named(NamedArgument { value, .. })) => 
                typecheck_pass(value, state)?
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

    Ok(Symbol::new(
        sym.value.clone(),
        type_stage_info(&sym.info, symbol_type, state)
    ))
}

/// See: https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L210
fn infer_single_func_call<'a>(
    call: &FuncCallSingle<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Result<FuncCallSingle<TypedStageInfo<'a>>> {
    let fn_sym = infer_symbol(&call.id, state)?;

    // t0 in the paper
    let mut current_typ = fn_sym.info.typ.clone();

    let mut arg_types = Vec::new();

    // TODO We have to sort the function arguments here for this to work with named arguments
    for arg in &call.args {
        // t1 in the paper
        let current_arg_typ = infer_argument(arg, state)?;
        arg_types.push(current_arg_typ.clone());

        // t' in the paper
        let ret_typ = state.hm.newvar();

        let constructed_func = Type::Fn(
            Box::new(current_arg_typ.info().typ.clone()),
            Box::new(ret_typ.clone())
        );

        let result = current_typ.unify(&constructed_func);

        if let Err(hmerror) = result {
            return unification_error(
                hmerror,
                &current_typ,
                &constructed_func,
                &call.info.inner
            );
        }

        current_typ = ret_typ;
    }

    Ok(FuncCallSingle::new(
        fn_sym,
        arg_types,
        type_stage_info(
            &call.info,
            current_typ,
            state
        )
    ))
}

fn infer_argument<'a>(
    arg: &Argument<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Result<Argument<TypedStageInfo<'a>>> {
    match arg {
        Argument::Positional(scoped_expr) => {
            let typed_expr = typecheck_pass(scoped_expr, state)?;

            Ok(Argument::Positional(typed_expr))
        },

        // TODO Checking the name?
        Argument::Named(
            NamedArgument { name: Symbol { value: name, .. },
                value: scoped_expr, .. }
        ) => {
            let expr = typecheck_pass(scoped_expr, state)?;

            Ok(Argument::Named(
                NamedArgument::new(
                    Symbol::new(name.clone(), expr.info().clone()),
                    expr.clone(),
                    expr.info().clone(),
                )
            ))
        }
    }
}

fn infer_array<'a>(
    barray: &Array<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Result<Array<TypedStageInfo<'a>>> {

    // TODO InteliJ doesn't like this for some reason
    let Some(first) = barray.value.first() else {
        return Ok(Array::new(
            vec![],
            type_stage_info(&barray.info, array(&state.hm.newvar()), state)
        ));
    };

    let cloned_values = barray.value.clone();

    let elem = typecheck_pass(first, state)?;

    let typed_values = cloned_values
        .into_iter()
        .map(|element| typecheck_pass(&element, state))
        .collect::<Result<Vec<Expr<TypedStageInfo>>>>()?;

    if typed_values.iter().any(|current| current.typ() != elem.typ()) {
        panic!("mixed types in array: {:?}", typed_values.iter().map(|c| c.typ()).collect::<Vec<_>>());
    }

    Ok(Array::new(
        typed_values,
        type_stage_info(
            &barray.info,
            array(&elem.typ().clone()),
            state
        )
    ))
}

fn infer_dict<'a>(
    dictionary: &Dict<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Result<Dict<TypedStageInfo<'a>>> {

    let Some(first) = dictionary.value.first() else {
        return Ok(Dict::new(
            vec![],
            type_stage_info(
                &dictionary.info,
                dict(&state.hm.newvar(), &state.hm.newvar()),
                state
            )
        ))
    }

    fn infer_dict_entry<'a>(
        entry: &DictEntry<ScopedStageInfo<'a>>,
        state: &mut InferenceState<'a>
    ) -> Result<DictEntry<TypedStageInfo<'a>>> {
        let key = typecheck_pass(&entry.key, state)?;
        let value = typecheck_pass(&entry.value, state)?;

        Ok(DictEntry::new(
            key.clone(),
            value.clone(),
            type_stage_info(
                &entry.info,
                pair(key.typ(), value.typ()),
                state
            )
        ))
    }

    let cloned_values = dictionary.value.clone();

    let entry_expr = infer_dict_entry(first, state)?;
    let entry_type = entry_expr.info.typ;

    let typed_values = cloned_values
        .iter()
        .map(|entry| infer_dict_entry(entry, state))
        .collect::<Result<Vec<DictEntry<TypedStageInfo>>>>()?;

    let any_type_deviates = typed_values
        .iter()
        .any(|current_entry|
            current_entry.info.typ != entry_type
        );

    if any_type_deviates {
        panic!();
    }

    Ok(Dict::new(
        typed_values,
        type_stage_info(
            &dictionary.info,
            dict(
                entry_expr.key.typ(),
                entry_expr.value.typ()
            ),
            state
        )
    ))
}

fn infer_lambda<'a>(
    lambda: &Lambda<ScopedStageInfo<'a>>,
    state: &mut InferenceState<'a>
) -> Result<Lambda<TypedStageInfo<'a>>> {
    let typed_body = typecheck_pass(&lambda.body, state)?;

    let typed_symbols = &typed_body.info().syms;

    let typed_args = lambda
        .clone()
        .args
        .into_iter()
        .map(|argument| {
            let info = argument.info();
            let Symbol { value, .. } = argument.into_def_argument_symbol();

            let maybe_typed_expr = typed_symbols.get(&value);
            

            let typ = match maybe_typed_expr {
                Some(typed_expr) => typed_expr.inst(&mut state.hm),

                None => state.hm.newvar()
            };

            let stage_info = type_stage_info(&info, typ.clone(), state);
            let symbol = Symbol::new(value, stage_info.clone());

            match argument {
                Argument::Positional(_) =>
                    Ok(Argument::Positional(Expr::Symbol(symbol))),

                Argument::Named(NamedArgument { value, .. }) => {
                    let default_value = typecheck_pass(&value, state)?;
                    let hmerror = typ.unify(default_value.typ());

                    if let Err(hmerror) = hmerror {
                        return unification_error(
                            hmerror,
                            &typ,
                            &default_value.typ(),
                            &Some(stage_info.inner) // TODO Please make this pretty
                        );
                    }

                    Ok(Argument::Named(
                        NamedArgument::new(
                        symbol,
                            default_value,
                            stage_info
                        )
                    ))
                }
            }
        })
        .collect::<Result<Vec<Argument<TypedStageInfo>>>>()?;

    let arg_types = typed_args
        .iter()
        .map(|arg| arg.info().typ.clone())
        .collect::<Vec<Type>>();

    let fun_type = func(&arg_types[..], typed_body.typ());

    Ok(Lambda::parametric(
        typed_args,

        typed_body,

        type_stage_info(&lambda.info, fun_type, state)
    ))
}


#[cfg(test)]
mod tests {
    use std::panic::catch_unwind;
    use crate::ast::parsed::{is_not_comment, parsed_expr_pass};
    use crate::ast::scoped::{scoped_expr_pass, ScopedStageInfo};
    use crate::library::Library;
    use crate::parser::{BunnyParser, Rule};
    use crate::types::hm::Type;
    use crate::types::typecheck_pass;
    use pest::Parser;
    use crate::ast::Expr;
    use crate::library;

    fn test_library<'a>() -> Library<'a> {
        library! {

        }
    }

    fn prepare_expr<'a>(expr: &'static str, library: &Library<'a>) -> Expr<ScopedStageInfo<'a>> {
        let mut pair = BunnyParser::parse(Rule::program, expr)
            .unwrap()
            .filter(is_not_comment)
            .next()
            .expect("no program :("); // TODO Write a utility function for this


        let basic_ast = parsed_expr_pass(pair.clone());
        scoped_expr_pass(basic_ast, &library.scoped)
    }

    fn assert_type(expr: &'static str, needed_type: Type) {
        let mut library = test_library();
        let scoped_ast = prepare_expr(expr, &library);
        let typed_ast = typecheck_pass(&scoped_ast, &mut library.typed)?;

        let result_type = typed_ast.typ();
        needed_type.unify(result_type);
    }

    fn assert_panics(expr: &'static str) {
        let maybe_panic = catch_unwind(|| {
            let mut library = test_library();
            let scoped_ast = prepare_expr(expr, &library);
            typecheck_pass(&scoped_ast, &mut library.typed)
        });

        assert!(maybe_panic.is_err(), "Panic-Test did not panic")
    }


}
