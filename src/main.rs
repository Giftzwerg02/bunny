pub mod ast;
pub mod cli;
pub mod debug;
pub mod parser;
mod types;
mod runner;
mod library;

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
use library::runnable_expression::RunnableExpr;
use parser::{BunnyParser, Rule};
use pest::Parser;
use runner::Runner;
use types::typed::TypedStageInfo;
use crate::ast::Symbol;
use crate::library::standard_library;
use crate::types::{typecheck_pass, InferenceState};
use crate::types::typed::{PolyTypedStageInfo, TypedValue};
use crate::types::util::{bfunc, bint, bstring};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string("src/parser/examples/never-say-never.bny")?;
    let mut pair = BunnyParser::parse(Rule::program, input.leak())?.filter(is_not_comment);
    let pair = pair.next().expect("no program :(");
    let ast = parsed_expr_pass(pair.clone());

    let mut std_library = standard_library();

    let ast = timed!(scoped_expr_pass(ast, &std_library.scoped));
    println!("{}", ast.pretty_print());

    let typ = typecheck_pass(&ast, &mut std_library.typed);
    println!("{}", typ.pretty_print());

    let typ = typ.map_stage(
        &mut |typed_info: TypedStageInfo| typed_info.generalize(&std_library.typed.hm)
    );

    let mut runner = Runner::new();
    let result = runner.run(typ, std_library.runnable);
    println!("lazy: {:?}", result);
    println!("{:?}", result.eval());

    Ok(())
}
