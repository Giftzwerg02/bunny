pub mod parser;

use parser::{BunnyParser, Rule};
use pest::Parser;

fn main() {
    let p = BunnyParser::parse(Rule::program, "1234");
    println!("{:?}", p);
}
