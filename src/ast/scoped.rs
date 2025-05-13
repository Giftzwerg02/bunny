use core::panic;
use std::fmt::Display;

use im::HashMap;
use miette::{LabeledSpan, Result, SourceSpan, miette};
use strsim::normalized_levenshtein;
use text_trees::StringTreeNode;

use crate::ast::{Argument, FuncCallSingle};

use super::{
    Array, Color, Dict, DictEntry, Expr, Float, FuncCall, Int, Lambda, NamedArgument,
    PrettyPrintable, StageInfo, Str, Symbol, parsed::ParsedStageInfo,
};

#[derive(Debug, Clone)]
pub struct ScopedStageInfo<'a> {
    pub inner: Option<ParsedStageInfo<'a>>,
    pub syms: SymbolTable<ScopedStageInfo<'a>>,
}

fn create_source(source: ParsedStageInfo) -> SourceSpan {
    let span = source.token.as_span();
    (span.start(), span.end() - span.start()).into()
}

fn find_similar_vars<'a>(wrong_name: &str, syms: &SymbolTable<ScopedStageInfo<'a>>) -> Vec<(String, SymbolValue<ScopedStageInfo<'a>>)> {
    fn is_similar(a: &str, b: &str, threshold: f64) -> bool {
        normalized_levenshtein(a, b) >= threshold
    }

    // NOTE: I carefully chose this value completely at random
    let threshold = 0.7;

    syms.inner.iter()
        .filter(|(k,_)| is_similar(wrong_name, k, threshold))
        .map(|(k,v)| (k.clone(), v.clone()))
        .collect()
}

impl<'a> ScopedStageInfo<'a> {
    pub fn new(inner: ParsedStageInfo<'a>, syms: SymbolTable<ScopedStageInfo<'a>>) -> Self {
        Self {
            inner: Some(inner),
            syms,
        }
    }

    /// Used for builtin / library functions.
    /// Doesn't provide a inner ParsedStageInfo, since they do not "appear" in the source code and
    /// therefore don't have a position / range (Pair).
    pub fn libinfo(syms: SymbolTable<ScopedStageInfo<'a>>) -> Self {
        Self { inner: None, syms }
    }
}

impl PrettyPrintable for ScopedStageInfo<'_> {
    fn pretty_print(&self) -> StringTreeNode {
        StringTreeNode::new(String::new())
    }
}

impl Display for ScopedStageInfo<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ syms: {} }}", self.syms)
    }
}

impl StageInfo for ScopedStageInfo<'_> {}

#[derive(Clone, Debug)]
pub enum SymbolValue<I: StageInfo> {
    Defined, // from a def
    Argument(Argument<I>),
    FunctionDefinition(Lambda<I>),
}

impl<I: StageInfo> SymbolValue<I> {
    fn name(&self) -> String {
        match self {
            SymbolValue::Defined => "DEFINED".to_string(),
            SymbolValue::FunctionDefinition(function_definition) => function_definition.name(),
            SymbolValue::Argument(argument) => argument.name(),
        }
    }
}

impl<I: StageInfo> From<Lambda<I>> for SymbolValue<I> {
    fn from(val: Lambda<I>) -> Self {
        SymbolValue::FunctionDefinition(val)
    }
}

#[derive(Clone, Debug)]
pub struct SymbolTable<I: StageInfo> {
    pub inner: HashMap<String, SymbolValue<I>>,
}

impl<I: StageInfo> Display for SymbolTable<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}",
            self.inner
                .iter()
                .map(|(k, v)| format!("({} -> {})", k, v.name()))
                .collect::<Vec<_>>()
        )
    }
}

impl<I: StageInfo> Default for SymbolTable<I> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: StageInfo> SymbolTable<I> {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn insert(&mut self, sym: String, value: SymbolValue<I>) {
        self.inner.insert(sym, value);
    }

    pub fn contains(&self, sym: &str) -> bool {
        self.inner.contains_key(sym)
    }

    pub fn get(&self, sym: &str) -> Option<&SymbolValue<I>> {
        self.inner.get(sym)
    }
}

pub fn scoped_expr_pass<'a, 'b: 'a>(
    src: Expr<ParsedStageInfo<'a>>,
    syms: &SymbolTable<ScopedStageInfo<'b>>,
) -> Result<Expr<ScopedStageInfo<'a>>> {
    let new_expr = match src {
        Expr::Int(int) => Expr::Int(Int::new(int.value, info(int.info, syms.clone()))),
        Expr::Float(float) => Expr::Float(Float::new(float.value, info(float.info, syms.clone()))),
        Expr::String(str) => Expr::String(Str::new(str.value, info(str.info, syms.clone()))),
        Expr::Color(color) => Expr::Color(Color::new(
            color.r,
            color.g,
            color.b,
            color.alpha,
            info(color.info, syms.clone()),
        )),
        Expr::Array(array) => {
            let info = info(array.info, syms.clone());
            let array = array
                .value
                .into_iter()
                .map(|v| scoped_expr_pass(v, syms))
                .collect::<Result<Vec<_>>>()?;

            Expr::Array(Array::new(array, info))
        }
        Expr::Dict(dict) => {
            let dict_info = info(dict.info, syms.clone());
            let dict = dict
                .value
                .into_iter()
                .map(|e| {
                    let k = scoped_expr_pass(e.key, syms)?;
                    let v = scoped_expr_pass(e.value, syms)?;
                    let dict_entry_info = info(e.info, syms.clone());
                    Ok(DictEntry::new(k, v, dict_entry_info))
                })
                .collect::<Result<Vec<_>>>()?;

            Expr::Dict(Dict::new(dict, dict_info))
        }
        Expr::FuncCall(func_call) => {
            match func_call {
                FuncCall::Single(func_call_single) => {
                    // 1. check if function exists
                    if !syms.contains(&func_call_single.id.value) {
                        let token = create_source(func_call_single.id.info);
                        let similar_vars = find_similar_vars(&func_call_single.id.value, syms);
                        let help = match &similar_vars[..] {
                            [] => format!("A definition is anything in the form of: (def {} ...)", func_call_single.id.value),
                            [(a, _)] => format!("A similar name was found: {a}"),
                            multiple => format!("Multiple similar names were found: {}", multiple.iter().map(|(s,_)| s.clone()).collect::<Vec<String>>().join(", "))
                        };


                        let mut labels = vec![LabeledSpan::at(token, "This name is not defined")];
                        for (s, val) in similar_vars {
                            if let SymbolValue::FunctionDefinition(lambda) = val {
                                let lambda_token = create_source(lambda.info.inner.unwrap());
                                let span = LabeledSpan::at(lambda_token, format!("\"{s}\" defined here"));
                                labels.push(span);
                            }
                        }

                        let report = miette!(
                            labels = labels,
                            help = help,
                            code = "undefined function call",
                            "Function \"{}\" is not defined.", func_call_single.id.value
                        );

                        return Err(report);
                    }

                    // 2. check if it is the def-function (special handling)
                    // (def <name> (<arg1> <arg2> <arg3>) (<body>))
                    // (def foo (x y z) (
                    //      + x y z
                    // ))
                    //
                    // syms.insert(foo, FuncCall(id: +, args: [x, y, z]))
                    //
                    //  ^ id  ^ arg       ^ arg (optional)   ^ arg
                    //  i.e., 2 or 3 args
                    if func_call_single.is_def() {
                        Expr::Lambda(handle_def(func_call_single, syms)?)
                    } else if func_call_single.is_lambda() {
                        Expr::Lambda(handle_lambda(func_call_single, syms)?)
                    } else {
                        let mapped_args = func_call_single
                            .args
                            .into_iter()
                            .map(|arg| pass_arg(arg, syms))
                            .collect::<Result<Vec<_>>>()?;

                        let ret = FuncCall::Single(FuncCallSingle::new(
                            pass_symbol(func_call_single.id, syms.clone()),
                            mapped_args,
                            info(func_call_single.info, syms.clone()),
                        ));

                        Expr::FuncCall(ret)
                    }
                }
                FuncCall::List(func_call_list) => {
                    assert!(
                        !func_call_list.calls.is_empty(),
                        "empty func-call-lists are not allowed"
                    );

                    let mut new_syms = syms.clone();
                    let mut out_func: Option<Expr<_>> = None;

                    for (i, call) in func_call_list.calls.iter().enumerate() {
                        let passed_call =
                            scoped_expr_pass(Expr::FuncCall(call.clone()), &new_syms)?;
                        new_syms = passed_call.info().syms.clone();

                        if i == func_call_list.calls.len() - 1 {
                            out_func = Some(passed_call);
                        }
                    }

                    out_func.unwrap()
                }
            }
        }
        Expr::Symbol(symbol) => {
            let s = Symbol::new(symbol.value.clone(), info(symbol.info.clone(), syms.clone()));
            if !syms.contains(&symbol.value) {
                let token = create_source(symbol.info);
                let similar_vars = find_similar_vars(&symbol.value, syms);
                let help = match &similar_vars[..] {
                    [] => format!("A definition is anything in the form of: (def {} ...)", symbol.value),
                    [(a, _)] => format!("A similar name was found: {a}"),
                    multiple => format!("Multiple similar names were found: {}", multiple.iter().map(|(s,_)| s.clone()).collect::<Vec<String>>().join(", "))
                };


                let mut labels = vec![LabeledSpan::at(token, "This name is not defined")];
                for (s, val) in similar_vars {
                    if let SymbolValue::FunctionDefinition(lambda) = val {
                        let lambda_token = create_source(lambda.info.inner.unwrap());
                        let span = LabeledSpan::at(lambda_token, format!("\"{s}\" defined here"));
                        labels.push(span);
                    }
                }

                let report = miette!(
                    labels = labels,
                    help = help,
                    code = "undefined function call",
                    "Function \"{}\" is not defined.", symbol.value
                );

                return Err(report);
            }
            Expr::Symbol(s)
        }
        Expr::Lambda(_) => panic!("shouldn't exist here"),
    };

    Ok(new_expr)
}

fn pass_symbol<'a>(
    symbol: Symbol<ParsedStageInfo<'a>>,
    table: SymbolTable<ScopedStageInfo<'a>>,
) -> Symbol<ScopedStageInfo<'a>> {
    Symbol::new(symbol.value.clone(), info(symbol.info.clone(), table))
}

fn info<'a>(
    parsed: ParsedStageInfo<'a>,
    syms: SymbolTable<ScopedStageInfo<'a>>,
) -> ScopedStageInfo<'a> {
    info_opt(Some(parsed), syms)
}

fn info_opt<'a>(
    parsed: Option<ParsedStageInfo<'a>>,
    syms: SymbolTable<ScopedStageInfo<'a>>,
) -> ScopedStageInfo<'a> {
    ScopedStageInfo {
        inner: parsed,
        syms,
    }
}

fn find_non_unique_args<I: StageInfo>(args: &[Argument<I>]) -> Vec<Argument<I>> {
    let mut found: HashMap<_, Vec<Argument<I>>> = HashMap::new();
    for arg in args {
        let sym = arg.into_def_argument_symbol();
        if found.contains_key(&sym.value) {
            found.get_mut(&sym.value).unwrap().push(arg.clone());
        } else {
            found.insert(sym.value, vec![arg.clone()]);
        }
    }

    found.into_iter()
        .filter(|(_,v)| v.len() > 1)
        .flat_map(|(_,v)| v)
        .collect()
}

fn pass_arg_def<'a>(
    arg: Argument<ParsedStageInfo<'a>>,
    syms: &SymbolTable<ScopedStageInfo<'a>>,
) -> Result<Argument<ScopedStageInfo<'a>>> {
    let res = match arg {
        Argument::Positional(_) => {
            let sym = pass_symbol(arg.into_def_argument_symbol(), syms.clone());
            Argument::Positional(Expr::Symbol(sym))
        }
        Argument::Named(named_argument) => {
            let name = pass_symbol(named_argument.name, syms.clone());
            let expr = *named_argument.value;
            let expr = scoped_expr_pass(expr, syms)?;
            Argument::Named(NamedArgument::new(
                name,
                expr.clone(),
                info_opt(expr.info().inner.clone(), syms.clone()),
            ))
        }
    };

    Ok(res)
}

fn pass_arg<'a>(
    arg: Argument<ParsedStageInfo<'a>>,
    syms: &SymbolTable<ScopedStageInfo<'a>>,
) -> Result<Argument<ScopedStageInfo<'a>>> {
    let res = match arg {
        Argument::Positional(expr) => Argument::Positional(scoped_expr_pass(expr, syms)?),
        Argument::Named(named_argument) => {
            let name = pass_symbol(named_argument.name, syms.clone());
            let expr = *named_argument.value;
            let expr = scoped_expr_pass(expr, syms)?;
            Argument::Named(NamedArgument::new(
                name,
                expr.clone(),
                info_opt(expr.info().inner.clone(), syms.clone()),
            ))
        }
    };

    Ok(res)
}

fn handle_def<'a, 'b: 'a>(
    def: FuncCallSingle<ParsedStageInfo<'a>>,
    syms: &SymbolTable<ScopedStageInfo<'b>>,
) -> Result<Lambda<ScopedStageInfo<'a>>> {
    // create a new sym-table entry with the newly defined value
    let Argument::Positional(ref new_id) = def.args[0] else {
        panic!("invalid ast");
    };

    let Expr::Symbol(new_id) = new_id else {
        panic!("invalid ast");
    };

    let mut inner_syms = syms.clone();
    let func_id = pass_symbol(new_id.clone(), inner_syms.clone());
    inner_syms.insert(func_id.clone().value, SymbolValue::Defined);

    let res = match &def.args[1..] {
        [new_call] => {
            let Argument::Positional(new_call) = new_call else {
                panic!("invalid ast");
            };

            let new_call = scoped_expr_pass(new_call.clone(), &inner_syms)?;

            // We just created a "constant" function, i.e., with no arguments
            // Therefore, the created function where the func_id should reference
            // (in the symbol table) a function with a singular argument, that
            // being the body of the function.
            let mut lambda = Lambda::constant(new_call.clone(), info(def.info, syms.clone()));
            lambda
                .info
                .syms
                .insert(func_id.value, lambda.clone().into());
            lambda
        }
        [func_args @ .., new_call] => {
            // We just created a parametric function, i.e., with n >= 1 arguments.
            // Therefore, the created function where the func_id should reference
            // (in the symbol table) a function with n+1 arguments, that being the
            // n arguments + the body of the function.

            let non_unique_args = find_non_unique_args(func_args);
            if !non_unique_args.is_empty() {
                let mut labels = vec![];
                for arg in non_unique_args {
                    let token = create_source(arg.info().clone());
                    let span = LabeledSpan::at(token, "dup");
                    labels.push(span);
                }

                let report = miette!(
                    labels = labels,
                    help = "Each argument in a function definition must have a unique name, e.g., (def foo (i am unique <3) ...)",
                    code = "duplicate argument names in definition",
                    "Multiple arguments inside of a function-definition have the same name."
                );

                return Err(report);
            }

            let func_args = func_args
                .iter()
                .map(|arg| pass_arg_def(arg.clone(), syms))
                .collect::<Result<Vec<_>>>()?;

            for arg in &func_args {
                let arg_sym = arg.into_def_argument_symbol();
                inner_syms.insert(arg_sym.value.clone(), SymbolValue::Argument(arg.clone()));
            }

            let Argument::Positional(new_call) = new_call else {
                panic!("invalid ast");
            };

            let new_call = scoped_expr_pass(new_call.clone(), &inner_syms)?;
            let new_call_args = func_args;

            let mut lambda = Lambda::parametric(
                new_call_args,
                new_call.clone(),
                info(def.info, syms.clone()),
            );
            lambda
                .info
                .syms
                .insert(func_id.value, lambda.clone().into());
            lambda
        }
        _ => panic!("invalid ast"),
    };

    Ok(res)
}

// A lambda is a function call in the form of: (\ (args...) (body))
// Meaning there are two cases:
//  1. (\ (body)) -> 1 argument which is just the function body
//  2. (\ (args...) (body)) -> 2 arguments, the first is the parameter-list, the second is the
//     function body
// Note: There is no `new_syms` like in the handle_def function since a lambda does not have a name!
fn handle_lambda<'a>(
    lambda: FuncCallSingle<ParsedStageInfo<'a>>,
    syms: &SymbolTable<ScopedStageInfo<'a>>,
) -> Result<Lambda<ScopedStageInfo<'a>>> {
    let res = match &lambda.args[..] {
        // Case 1: Only function body
        [new_call] => {
            let Argument::Positional(new_call) = new_call else {
                panic!("invalid ast");
            };

            let new_call = scoped_expr_pass(new_call.clone(), syms)?;
            Lambda::constant(new_call, info(lambda.info, syms.clone()))
        }
        // Case 2: Parameters and Function-Body
        [func_args @ .., new_call] => {
            let mut inner_syms = syms.clone();

            // We just created a parametric lambda, i.e., with n >= 1 arguments.
            // Therefore, the created function where the func_id should reference
            // (in the symbol table) a function with n+1 arguments, that being the
            // n arguments + the body of the function.

            let non_unique_args = find_non_unique_args(func_args);
            if !non_unique_args.is_empty() {
                let mut labels = vec![];
                for arg in non_unique_args {
                    let token = create_source(arg.info().clone());
                    let span = LabeledSpan::at(token, "dup");
                    labels.push(span);
                }

                let report = miette!(
                    labels = labels,
                    help = "Each argument in a function definition must have a unique name, e.g., (def foo (i am unique <3) ...)",
                    code = "duplicate argument names in definition",
                    "Multiple arguments inside of a function-definition have the same name."
                );

                return Err(report);
            }

            let func_args = func_args
                .iter()
                .map(|arg| pass_arg_def(arg.clone(), syms))
                .collect::<Result<Vec<_>>>()?;

            for arg in &func_args {
                let arg_sym = arg.into_def_argument_symbol();
                inner_syms.insert(arg_sym.value.clone(), SymbolValue::Argument(arg.clone()));
            }

            let Argument::Positional(new_call) = new_call else {
                panic!("invalid ast");
            };

            let new_call = scoped_expr_pass(new_call.clone(), &inner_syms)?;
            let args = func_args;

            Lambda::parametric(args, new_call, info(lambda.info, syms.clone()))
        }
        _ => panic!("invalid ast"),
    };

    Ok(res)
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{self, parsed::parsed_expr_pass},
        parser::{BunnyParser, Rule},
    };

    use super::*;
    use pest::Parser;

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

    fn scoped_test<'a>(code: &str) -> miette::Result<()> {
        let pair = BunnyParser::parse(Rule::program, code)
            .unwrap()
            .next()
            .unwrap();

        let mut syms = SymbolTable::new();
        let info = ScopedStageInfo::libinfo(syms.clone());

        syms.insert("def".to_string(), SymbolValue::Defined);
        syms.insert(r"\".to_string(), SymbolValue::Defined);
        let empty = Lambda::constant(empty_func_expr(info.clone()), info);
        syms.insert("+".to_string(), empty.into());

        let parsed_expr = parsed_expr_pass(pair);
        scoped_expr_pass(parsed_expr, &syms)
            .map_err(|report| {
                report.with_source_code(code.to_string())
            })?;
        Ok(())
    }

    fn scoped_panic_test(code: &str) {
        let pair = BunnyParser::parse(Rule::program, code)
            .unwrap()
            .next()
            .unwrap();

        let mut syms = SymbolTable::new();
        let info = ScopedStageInfo::libinfo(syms.clone());

        syms.insert("def".to_string(), SymbolValue::Defined);
        syms.insert(r"\".to_string(), SymbolValue::Defined);
        let empty = Lambda::constant(empty_func_expr(info.clone()), info);
        syms.insert("+".to_string(), empty.into());

        let parsed_expr = parsed_expr_pass(pair);
        let result = scoped_expr_pass(parsed_expr, &syms);
        assert!(result.is_err(), "expected code: {}\nto error", code)
    }

    #[test]
    fn defined_variables_can_be_referenced_later() -> Result<()> {
        scoped_test(
            "
            (
                (def a 5)
                (a)
            )
        ",
        )
    }

    #[test]
    fn defined_variables_cannot_be_referenced_earlier() {
        scoped_panic_test(
            "
            (
                (a)
                (def a 5)
            )
        ",
        )
    }

    #[test]
    fn undefined_variables_cannot_be_referenced() {
        scoped_panic_test(
            "
            (
                (def b 5)
                (a)
            )
        ",
        )
    }

    #[test]
    fn scoped_variables_cannot_be_referenced_out_of_scope() {
        scoped_panic_test(
            "(
            (def a (
                (def b 5)
            ))
            (b)
        )",
        );
    }

    #[test]
    fn arguments_can_be_accessed_within_a_funccall() -> Result<()> {
        scoped_test(
            "(
            (def a (x y z) (
                (+ x y z)
            ))
        )",
        )
    }

    #[test]
    fn arguments_cannot_be_accessed_outside_a_funccall() {
        scoped_panic_test(
            "(
            (def a (x y z) (
                (+ x y z)
            ))
            (x)
        )",
        );

        scoped_panic_test(
            "(
            (def a (x y z) (
                (+ x y z)
            ))
            (y)
        )",
        );

        scoped_panic_test(
            "(
            (def a (x y z) (
                (+ x y z)
            ))
            (z)
        )",
        );

        scoped_panic_test(
            "(
            (x)
            (def a (x y z) (
                (+ x y z)
            ))
        )",
        );

        scoped_panic_test(
            "(
            (y)
            (def a (x y z) (
                (+ x y z)
            ))
        )",
        );

        scoped_panic_test(
            "(
            (z)
            (def a (x y z) (
                (+ x y z)
            ))
        )",
        );
    }

    #[test]
    fn arguments_can_be_accessed_inside_a_nested_scope() -> Result<()> {
        scoped_test(
            "(
            (def a (x y z) (
                (def b (foo bar baz) (
                    (+ x y z foo bar baz)
                ))
                (b 4 5 6)
            ))
            (a 1 2 3)
        )",
        )
    }

    #[test]
    fn variables_can_be_accessed_inside_a_nested_scope() -> Result<()> {
        scoped_test(
            "(
            (def foo (bar baz) (
                (+ bar baz)
            ))
            (def a (x y z) (
                (+ x y z (foo 3 4))
            ))
            (a 1 2 3)
        )",
        )
    }

    #[test]
    fn arguments_can_be_set_by_their_name() -> Result<()> {
        scoped_test(
            "(
                (def a (foo bar baz) (+ foo bar baz))
                (a foo: 1 bar: 2 baz: 3)
        )",
        )
    }

    #[test]
    fn defined_variables_can_be_used_as_return_values() -> Result<()> {
        scoped_test(
            "(
            (def a 5)
            (def b a)
            (b)
        )",
        )
    }

    #[test]
    fn undefined_variables_cannot_be_used_as_return_values() {
        scoped_panic_test(
            "(
            (def b a)
            (b)
        )",
        );
    }

    #[test]
    fn variables_cannot_be_accessed_from_a_dynamic_successor() {
        scoped_panic_test(
            "
            (
                (def a (b) (
                    (def inner 5) 
                    (+ 1 b inner)
                ))
                (def foo (bar) (
                    (+ 2 bar inner)
                ))
                (a (foo 3)) 
            )
        ",
        );

        scoped_panic_test(
            "
            (
                (def a (b) (
                    (def inner 5) 
                    (+ 1 b inner)
                ))
                (def foo (bar) (
                    (+ 2 bar b)
                ))
                (a (foo 3)) 
            )
        ",
        );

        scoped_panic_test(
            "
            (
                (def foo (bar) (
                    (def inner 5)
                    (+ 1 2 inner bar)
                ))
                (def a (b) (
                    (def res (foo 1)) 
                    (+ inner res b)
                ))
                (a 4) 
            )
        ",
        );

        scoped_panic_test(
            "
            (
                (def foo (bar) (
                    (def inner 5)
                    (+ 1 2 inner bar)
                ))
                (def a (b) (
                    (def res (foo 1)) 
                    (+ bar res b)
                ))
                (a 4) 
            )
        ",
        );
    }

    #[test]
    fn lambda_arguments_can_be_accessed_within_a_funccall() -> Result<()> {
        scoped_test(
            r"(
            (\ (x y z) (
                (+ x y z)
            ))
        )",
        )
    }

    #[test]
    fn lambda_arguments_cannot_be_accessed_outside_a_funccall() {
        scoped_panic_test(
            r"(
            (\ (x y z) (
                (+ x y z)
            ))
            (x)
        )",
        );

        scoped_panic_test(
            r"(
            (\ (x y z) (
                (+ x y z)
            ))
            (y)
        )",
        );

        scoped_panic_test(
            r"(
            (\ (x y z) (
                (+ x y z)
            ))
            (z)
        )",
        );

        scoped_panic_test(
            r"(
            (x)
            (\ (x y z) (
                (+ x y z)
            ))
        )",
        );

        scoped_panic_test(
            r"(
            (y)
            (\ (x y z) (
                (+ x y z)
            ))
        )",
        );

        scoped_panic_test(
            r"(
            (z)
            (\ (x y z) (
                (+ x y z)
            ))
        )",
        );
    }

    #[test]
    fn lambda_arguments_can_be_accessed_inside_a_nested_scope() -> Result<()> {
        scoped_test(
            r"(
            (\ (x y z) (
                (\ (foo bar baz) (
                    (+ x y z foo bar baz)
                ))
            ))
        )",
        )
    }

    #[test]
    fn previous_defs_can_be_used_inside_lambdas() -> Result<()> {
        scoped_test(
            r"
            (
                (def a 5)
                (\ (b) (
                    + a b
                ))
            )
            ",
        )
    }

    #[test]
    fn defs_and_lambdas_can_be_nested_together() -> Result<()> {
        scoped_test(
            r"
            (
                (def a (x y z) (
                    (\ (foo bar baz) (
                        (def test 5)
                        (+ test x y z foo bar baz)
                    ))
                )) 
                (a 1 2 3)
            )
        ",
        )
    }

    #[test]
    fn def_parameters_must_be_unique() {
        scoped_panic_test(
            r"
            (
                (def a (b b) (
                    (+ b b)
                ))
                (a 1 2)
            ) 
        ",
        );

        scoped_panic_test(
            r"
            (
                (def a (foo bar b b baz boo) (
                    (+ b b)
                ))
                (a 1 2 3 4 5 6)
            ) 
        ",
        );
    }

    #[test]
    fn def_parameters_may_be_shadowed() -> Result<()> {
        scoped_test(
            r"
            (
                (def a (bla) (
                    (def inner (bla) (
                        (+ 3 bla)
                    ))
                    (+ (inner bla: 4) 6)
                ))
                (+ 4 (a bla: 12))
            )
        ",
        )
    }

    #[test]
    fn lambda_parameter_names_must_be_unique() {
        scoped_panic_test(
            r"
            (
                (def a (
                    (\ (b b) (
                        (+ b b)
                    ))
                ))
                (a)
            ) 
        ",
        );

        scoped_panic_test(
            r"
            (
                (\ (foo bar b b baz boo) (
                    (+ b b)
                ))
            ) 
        ",
        );
    }

    #[test]
    fn lambda_parameters_may_be_shadowed() -> Result<()> {
        scoped_test(
            r"
            (
                (\ (bla) (
                    (\ (bla) (
                        (+ 3 bla)
                    ))
                ))
            )
        ",
        )
    }

    #[test]
    fn def_and_lambda_parameters_may_be_shadowed() -> Result<()> {
        scoped_test(
            r"
            (
                (def a (bla) (
                    (\ (bla) (
                        (def a (bla) (
                            (+ 10 bla)
                        ))
                        (a bla: 1)
                    ))
                ))
                (+ 30 (a bla: 3))
            )
        ",
        )
    }

    #[test]
    fn def_default_arguments_may_appear_in_any_order_with_non_default_args() -> Result<()> {
        scoped_test(
            r"
            (
                (def foo (a: 5 b c: 2) (+ a (+ b c)))
            )
            ",
        )
    }
}
