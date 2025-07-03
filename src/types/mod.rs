pub mod typed;
pub mod hm;
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
pub struct InferenceState {
    pub type_assumptions: TypedSymbolTable,
    pub hm: HMState
}

impl InferenceState {
    pub fn new() -> Self {
        InferenceState {
            type_assumptions: TypedSymbolTable::new(),
            hm: HMState::new()
        }
    }

    pub fn scope<T>(&mut self, callback: impl FnOnce(&mut InferenceState) -> T) -> T {
        let old = self.type_assumptions.clone();

        let result = callback(self);

        self.type_assumptions = old;

        result
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

pub fn typecheck_pass(
    expr: &Expr<ScopedStageInfo>,
    state: &mut InferenceState
) -> Result<Expr<TypedStageInfo>> {
    let new_expr = match expr {
        Expr::Int(Int { value, info }) =>
            Expr::Int(
                Int::new(
                    *value,
                    type_stage_info(info, int(), state)
                )
            ),

        Expr::Float(Float { value, info }) =>
            Expr::Float(
                Float::new(
                    *value,
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

        Expr::Color(Color { r, g, b, alpha, info }) =>
            Expr::Color(
                Color::new(
                    *r, *g, *b, *alpha,
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
            infer_lambda(lambda, state)?,
    };

    Ok(new_expr)
}

fn type_stage_info(
    info: &ScopedStageInfo,
    typ: Type,
    state: &mut InferenceState
) -> TypedStageInfo {
    TypedStageInfo {
        inner: info.inner.clone().expect("to be called only with bunny expressions"),
        typ,
        syms: state.type_assumptions.clone()
    }
}

fn create_argument_definition(
    sym: &Symbol<ScopedStageInfo>,
    state: &mut InferenceState
) -> Symbol<TypedStageInfo> {
    Symbol::new(
        sym.value.clone(),
        type_stage_info(
            &sym.info,
            state.hm.newvar(),
            state
        )
    )
}

fn custom_type_error<T>(
    help: String,
    source: &Option<ParsedStageInfo>
) -> Result<T> {
    let token = source.clone().map(|source| {
        let span = source.token.as_span();
        (span.start(), span.end() - span.start()).into()
    });

    Err(TypeError {
        token,
        advice: help
    }.into())
}

fn type_error<T>(
    hmerror: HMError,
    source: &Option<ParsedStageInfo>
) -> Result<T> {
    match hmerror {
        HMError::UnificationError(a, b) => 
            custom_type_error(
                format!("Types {a} and {b} are not compatible"),
                source
            ),

        HMError::OccursCheckError(typ) => 
            custom_type_error(
                format!("Creating an infinite type with {typ} is not allowed"),
                source
            )
    }
}

/// Refer to https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L197
fn infer_symbol(
    sym: &Symbol<ScopedStageInfo>,
    state: &mut InferenceState
) -> Result<Symbol<TypedStageInfo>> {
    let key = &sym.value;

    // defs are infered as let-binded vars:
    // https://github.com/jfecher/algorithm-j/blob/7119150ae1822deac1dfe1dbb14f172d7c75e921/j.ml#L241
    if !state.type_assumptions.contains_key(key) {
        let scoped_expr = sym.info.syms.get(key)
            .unwrap_or_else(|| panic!("The should be no undefined symbols in the type checking stage: {key}"));

        state.hm.enter_level();

        let typed_expr = match scoped_expr {
            SymbolValue::Argument(Argument::Positional(_)) |
            SymbolValue::Defined =>
                // TODO Uhhh, when is this used now?
                Expr::Symbol(create_argument_definition(sym, state)),

            SymbolValue::FunctionDefinition(lambda) => 
                state.scope(|state| infer_lambda(lambda, state))?,

            SymbolValue::Argument(Argument::Named(NamedArgument { name, value, .. })) => {
                typecheck_pass(value, state)?;
                Expr::Symbol(create_argument_definition(name, state))
            }
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
fn infer_single_func_call(
    call: &FuncCallSingle<ScopedStageInfo>,
    state: &mut InferenceState
) -> Result<FuncCallSingle<TypedStageInfo>> {
    let fn_sym = infer_symbol(&call.id, state)?;

    // t0 in the paper
    let mut current_typ = fn_sym.info.typ.clone();

    let mut arg_types = Vec::new();

    // TODO We have to sort the function arguments here for this to work with named arguments
    for arg in &call.args {
        // t1 in the paper
        let current_arg_typ = infer_argument(arg, state)?;
        arg_types.push(current_arg_typ.clone());
        
        let arg_typ = current_arg_typ.info().typ.clone();

        // t' in the paper
        let ret_typ = state.hm.newvar();

        let constructed_func = Type::Fn(
            Box::new(arg_typ),
            Box::new(ret_typ.clone())
        );

        let result = current_typ.unify(&constructed_func);

        if let Err(hmerror) = result {
            return type_error(
                hmerror,
                &Some(current_arg_typ.info().inner.clone())
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

fn infer_argument(
    arg: &Argument<ScopedStageInfo>,
    state: &mut InferenceState
) -> Result<Argument<TypedStageInfo>> {
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

fn infer_array(
    barray: &Array<ScopedStageInfo>,
    state: &mut InferenceState
) -> Result<Array<TypedStageInfo>> {

    // TODO InteliJ doesn't like this for some reason
    let Some(first) = barray.value.first() else {
        return Ok(Array::new(
            vec![],
            type_stage_info(&barray.info, array(&state.hm.newvar()), state)
        ));
    };

    let cloned_values = barray.value.clone();

    let elem = typecheck_pass(first, state)?;

    // TODO This checks the first element twice
    let typed_values = cloned_values
        .iter()
        .map(|element| typecheck_pass(element, state))
        .collect::<Result<Vec<Expr<TypedStageInfo>>>>()?;

    for current in &typed_values {
        let hmerror = elem.typ().unify(current.typ());

        if hmerror.is_err() {
            return custom_type_error(
                format!("Cannot have element of type {} in array of type {}", current.typ(), elem.typ()),
                &Some(current.info().inner.clone())
            );
        }
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

fn infer_dict(
    dictionary: &Dict<ScopedStageInfo>,
    state: &mut InferenceState
) -> Result<Dict<TypedStageInfo>> {

    let Some(first) = dictionary.value.first() else {
        return Ok(Dict::new(
            vec![],
            type_stage_info(
                &dictionary.info,
                dict(&state.hm.newvar(), &state.hm.newvar()),
                state
            )
        ))
    };

    fn infer_dict_entry(
        entry: &DictEntry<ScopedStageInfo>,
        state: &mut InferenceState
    ) -> Result<DictEntry<TypedStageInfo>> {
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

    for current in &typed_values {
        let hmerror = entry_type.unify(&current.info.typ);

        if hmerror.is_err() {
            return custom_type_error(
                format!("Cannot have dict entry of type {} in array of type {}", current.info.typ, entry_type),
                &Some(current.info.inner.clone())
            );
        }
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

fn infer_lambda(
    lambda: &Lambda<ScopedStageInfo>,
    state: &mut InferenceState
) -> Result<Expr<TypedStageInfo>> {
    if lambda.args.is_empty(){
        return typecheck_pass(&lambda.body, state);
    }

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

            let stage_info = type_stage_info(info, typ.clone(), state);
            let symbol = Symbol::new(value, stage_info.clone());

            match argument {
                Argument::Positional(_) =>
                    Ok(Argument::Positional(Expr::Symbol(symbol))),

                Argument::Named(NamedArgument { value, info, .. }) => {
                    let default_value = typecheck_pass(&value, state)?;
                    let hmerror = typ.unify(default_value.typ());

                    if let Err(hmerror) = hmerror {
                        return type_error(
                            hmerror,
                            &info.inner
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

    Ok(Expr::Lambda(Lambda::parametric(
        typed_args,

        typed_body,

        type_stage_info(&lambda.info, fun_type, state)
    )))
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::parsed::parsed_expr_pass;
    use crate::ast::scoped::scoped_expr_pass;
    use crate::parser::pest_parsing_pass;
    use crate::library::Library;
    use crate::types::util::{func1, func2};
    use crate::library;

    // Helper function for parsing and typechecking bunny source
    fn typecheck_bunny_source(source: &str, library: &mut Library) -> Result<Type> {
        let peg = pest_parsing_pass(source.to_string())?;
        let ast = parsed_expr_pass(peg)?;
        let scoped_ast = scoped_expr_pass(ast, &library.scoped)?;
        let typed_ast = typecheck_pass(&scoped_ast, &mut library.typed)?;

        Ok(typed_ast.info().typ.clone())
    }

    fn assert_type_simple(source: &str, expected: Type) {
        assert_type(source, |_| expected);
    }

    fn assert_type(source: &str, expected: impl FnOnce(&mut HMState) -> Type){
        let mut lib = basic_library();
        let Ok(result) = typecheck_bunny_source(source, &mut lib) else {
            panic!("Failed to typecheck '{}'", source);
        };

        let expected_value = expected(&mut lib.typed.hm);
        let Ok(()) = result.unify(&expected_value) else {
            panic!("Type mismatch for '{}': expected {:?}, got {:?}", source, expected_value, result);
        };
    }

    // Helper to create a library with basic arithmetic operations
    fn basic_library() -> Library {
        library! {
            #[| _a:int() => _b:int() => @int()]
            fn "add"(Lazy::Int(_a), Lazy::Int(_b)) { unimplemented!(); }

            #[| _a:int() => _b:int() => @int()]
            fn "sub"(Lazy::Int(_a), Lazy::Int(_b)) { unimplemented!(); }

            #[forall a | _arr:array(&a) => _b:a => @a]
            fn "get"(_arr, _b) { unimplemented!(); }

            #[forall a | _elem:a => @a]
            fn "id"(_elem) { unimplemented!(); }

            #[forall a, b, c | _f1:func1(&a, &b) => _f2:func1(&b, &c) => @func1(&a, &c)]
            fn "compose1"(Lazy::Lambda(_f1), Lazy::Lambda(_f2)) { unimplemented!(); }

            #[forall a, b | _f:func1(&a, &b) => _input:a => @b]
            fn "apply1"(Lazy::Lambda(_f), _input) { unimplemented!(); }

            #[forall a | _arr:array(&a) => @a ]
            fn "first"(Lazy::Array(_arr)) { unimplemented!(); }
        }
    }

    #[test]
    fn test_basic_types() {
        // Test basic integer type
        assert_type_simple("(id 42)", int());

        // Test basic float type
        assert_type_simple("(id 3.14f)", float());

        // Test basic string type
        assert_type_simple("(id \"hello\")", string());

        // Test basic color type
        assert_type_simple("(id #ff0000)", color());
    }

    #[test]
    fn test_array_types() {
        // Test homogeneous integer array
        assert_type_simple("(id [1 2 3])", array(&int()));

        // Test homogeneous string array
        assert_type_simple("(id [\"a\" \"b\" \"c\"])", array(&string()));

        // Test empty array (should have type variable)
        assert_type("(id [])", |state| array(&state.newvar()));
    }

    #[test]
    fn test_heterogeneous_array_error() {
        let mut lib = basic_library();

        // Test heterogeneous array should fail
        let result = typecheck_bunny_source("(id [1 \"hello\"])", &mut lib);
        assert!(result.is_err());
    }

    #[test]
    fn test_dict_types() {
        // Test homogeneous dictionary
        assert_type_simple("(id [\"a\": 1 \"b\": 2])", dict(&string(), &int()));

        assert_type("(id [:])", |state| dict(&state.newvar(), &state.newvar()));
    }

    #[test]
    fn test_function_calls() {
        // Test simple function call
        assert_type_simple("(add 1 2)", int());

        // Test nested function calls
        assert_type_simple("(add (sub 2 3) (sub 10 5))", int());
    }

    #[test]
    fn test_lambda_expressions() {
        assert_type("(id (\\ (x) x))", |state| {
            let x = state.newvar();
            func1(&x, &x)
        });

        assert_type("(id (\\ (x y) x))", |state| {
            let x = state.newvar();
            let y = state.newvar();
            func2(&x, &x, &y)
        });
    }

    #[test]
    fn test_lambda_with_arithmetic() {
        // Test lambda that uses arithmetic
        assert_type_simple("(\\ x (add x 1))", func(&[int()], &int()));
    }

    #[test]
    fn test_variable_definitions() {
        // Test simple variable definition
        assert_type_simple("(def x 42)", int());

        // Test function definition
        assert_type_simple("(def double (\\ x (sub x 2)))", func(&[int()], &int()));
    }

    #[test]
    fn test_type_inference_with_variables() {
        // Test that variables get proper types inferred
        assert_type_simple("(def f (\\ x (add x x)))", func(&[int()], &int()));
    }

    #[test]
    fn test_let_polymorphism() {
        assert_type_simple("(get (id [1 2 3]) (id 0))", int());

        assert_type_simple("(first (first [[0]]))", int());
    }

    #[test]
    fn test_complex_inference(){
        assert_type("(def first2d (compose1 first first))", |state| {
            let a = state.newvar();
            func1(&array(&array(&a)), &a)
        });

        assert_type_simple("(apply1 (compose1 first first) (id [[1 2] [3 4]]))", int());
    }

    #[test]
    fn test_nested_array_types() {
        // Test nested arrays
        assert_type_simple("(id [[1 2] [3 4]])", array(&array(&int())));

        // Test deeply nested arrays
        assert_type_simple("(id [[[\"a\"]]])", array(&array(&array(&string()))));
    }

    #[test]
    fn test_function_type_errors() {
        let lib = basic_library();

        // Test calling function with wrong argument types
        let result = typecheck_bunny_source("(add 1 \"hello\")", &mut lib.clone());
        assert!(result.is_err());
    }
}
