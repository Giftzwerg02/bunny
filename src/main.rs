pub mod ast;
pub mod cli;
pub mod parser;
// mod library;

// mod types;
// mod runner;

use std::fs::{self};

use ast::{parsed::{filter_comments, parsed_expr_pass, ParsedStageInfo}, scoped::{scoped_expr_pass, ScopedStageInfo, SymbolTable}, Expr, Int, PrettyPrintable};
#[allow(unused)]
use clap::Parser as ClapParser;
#[allow(unused)]
use cli::Cli;
use parser::{BunnyParser, Rule};
use pest::Parser;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string("src/parser/examples/single-call.bny")?;
    let mut pair = BunnyParser::parse(Rule::program, &input)?
        .filter(filter_comments);
    let pair = pair.next().expect("no program :(");
    let ast = parsed_expr_pass(pair.clone());

    let mut syms = SymbolTable::new();
    syms.insert("def".to_string(), Expr::Int(
        Int::new(69, ScopedStageInfo::new(ParsedStageInfo::new(pair.clone()), syms.clone()))
    ));
    syms.insert("+".to_string(), Expr::Int(
        Int::new(69, ScopedStageInfo::new(ParsedStageInfo::new(pair), syms.clone()))
    ));

    let ast = scoped_expr_pass(ast, &syms);
    println!("{}", ast.pretty_print());
    Ok(())
}
