pub mod ast;
pub mod cli;
pub mod parser;
pub mod debug;
// mod types;
// mod library;

// mod types;
// mod runner;

use std::fs::{self};

use ast::{
    parsed::{filter_comments, parsed_expr_pass, ParsedStageInfo}, scoped::{scoped_expr_pass, ScopedStageInfo, SymbolTable}, Expr, PrettyPrintable, StageInfo
};
#[allow(unused)]
use clap::Parser as ClapParser;
#[allow(unused)]
use cli::Cli;
use parser::{BunnyParser, Rule};
use pest::Parser;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string("src/parser/examples/single-call.bny")?;
    let mut pair = BunnyParser::parse(Rule::program, &input)?.filter(filter_comments);
    let pair = pair.next().expect("no program :(");
    let ast = parsed_expr_pass(pair.clone());

    let mut syms = SymbolTable::new();
    let info = ScopedStageInfo::new(ParsedStageInfo::new(pair.clone()), syms.clone());

    syms.insert(
        "def".to_string(),
        empty_func_expr(info.clone())
    );

    syms.insert(
        "+".to_string(),
        empty_func_expr(info.clone())
    );

    let ast = timed!(scoped_expr_pass(ast, &syms));
    println!("{}", ast.pretty_print());
    Ok(())
}

fn empty_func<I: StageInfo>(info: I) -> ast::FuncCallSingle<I> {
    ast::FuncCallSingle::new(
        ast::Symbol::new("".to_owned(), info.clone()),
        vec![],
        info.clone()
    )
}

fn empty_func_expr<I: StageInfo>(info: I) -> Expr<I> {
    Expr::FuncCall(ast::FuncCall::Single(empty_func(info)))
}
