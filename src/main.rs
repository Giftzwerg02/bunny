pub mod ast;
pub mod cli;
pub mod parser;

use std::fs::{self};

#[allow(unused)]
use clap::Parser as ClapParser;
#[allow(unused)]
use cli::Cli;
use parser::{BunnyParser, Rule};
use pest::Parser;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string("src/parser/examples/simple.bny")?;
    BunnyParser::parse(Rule::program, &input)?;
    Ok(())
}
