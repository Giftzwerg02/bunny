pub mod parser;
pub mod cli;

use clap::Parser as ClapParser;
use cli::Cli;
use parser::{BunnyParser, Rule};
use pest::Parser;

fn main() {
    let cli = Cli::parse();
    println!("{:?}", cli);

    let p = BunnyParser::parse(Rule::program, "1234");
    println!("{:?}", p);
}
