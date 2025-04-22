pub mod ast;
pub mod cli;
pub mod debug;
pub mod parser;
mod types;
mod runner;
//mod library;

use std::fs::{self};

use ast::{
    Expr, PrettyPrintable, StageInfo,
    parsed::{is_not_comment, parsed_expr_pass},
    scoped::{ScopedStageInfo, SymbolTable, scoped_expr_pass},
};
#[allow(unused)]
use clap::Parser as ClapParser;
#[allow(unused)]
use cli::Cli;
use parser::{BunnyParser, Rule};
use pest::Parser;
use crate::ast::Symbol;
use crate::types::{typecheck_pass, InferenceState};
use crate::types::typed::PolyTypedStageInfo;
use crate::types::util::{func_type, int_type, string_type};
// use crate::types::{typecheck_pass, InferenceState};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string("src/parser/examples/single-call.bny")?;
    let mut pair = BunnyParser::parse(Rule::program, &input)?.filter(is_not_comment);
    let pair = pair.next().expect("no program :(");
    let ast = parsed_expr_pass(pair.clone());

    let mut syms = SymbolTable::new();
    let info = ScopedStageInfo::libinfo(syms.clone());

    syms.insert("def".to_string(), ast::scoped::SymbolValue::Defined);

    let empty = ast::Lambda::constant(empty_func_expr(info.clone()), info);
    syms.insert("+".to_string(), empty.clone().into());

    let ast = timed!(scoped_expr_pass(ast, &syms));
    println!("{}", ast.pretty_print());

    // Type checking
    //
    let mut inference_state = InferenceState::new();

    let add_type = func_type(
        vec![int_type(), int_type()],
        int_type()
    );

    inference_state.type_assumptions.insert("+".to_string(), Expr::Symbol(
        Symbol::new(
            "+".to_owned(),
            PolyTypedStageInfo {
                inner: empty.info.inner,
                typ: add_type.generalize(&inference_state.hm),
                syms: Default::default(),
            }
        )
    ));

    let typ = typecheck_pass(&ast, &mut inference_state);
    println!("{}", typ.pretty_print());

    Ok(())
}

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
