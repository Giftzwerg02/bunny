pub mod ast;
pub mod cli;
pub mod debug;
pub mod parser;
// mod types;
// mod types;
// mod library;
// mod runner;

use std::fs::{self};

use ast::{
    Expr, PrettyPrintable, StageInfo,
    parsed::{ParsedStageInfo, is_not_comment, parsed_expr_pass},
    scoped::{ScopedStageInfo, SymbolTable, scoped_expr_pass},
};
#[allow(unused)]
use clap::Parser as ClapParser;
#[allow(unused)]
use cli::Cli;
use parser::{BunnyParser, Rule};
use pest::Parser;
// use crate::types::{typecheck_pass, InferenceState};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string("src/parser/examples/single-call.bny")?;
    let mut pair = BunnyParser::parse(Rule::program, &input)?.filter(is_not_comment);
    let pair = pair.next().expect("no program :(");
    let ast = parsed_expr_pass(pair.clone());

    let mut syms = SymbolTable::new();
    let info = ScopedStageInfo::new(ParsedStageInfo::new(pair.clone()), syms.clone());

    syms.insert("def".to_string(), ast::scoped::SymbolValue::Defined);

    let empty = ast::Lambda::constant(empty_func_expr(info.clone()), info);
    syms.insert("+".to_string(), empty.into());

    let ast = timed!(scoped_expr_pass(ast, &syms));
    println!("{}", ast.pretty_print());

    //let mut inference_state = InferenceState::new();
    //let typ = typecheck_pass(&ast, &mut inference_state);
    //println!("{:?}", typ);

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
