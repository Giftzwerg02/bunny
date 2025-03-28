pub mod ast;
pub mod cli;
pub mod parser;
mod library;

mod types;
mod runner;

use std::fs::{self};

use ast::{filter_comments, parsed_expr_pass, Expr};
#[allow(unused)]
use clap::Parser as ClapParser;
#[allow(unused)]
use cli::Cli;
use parser::{BunnyParser, Rule};
use pest::Parser;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string("src/parser/examples/simple.bny")?;
    let mut pair = BunnyParser::parse(Rule::program, &input)?
        .filter(filter_comments);
    let pair = pair.next().expect("no program :(");
    let ast = parsed_expr_pass(pair);
    println!("{}", ast.pretty_print());
    Ok(())
}
