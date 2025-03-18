pub mod parser;

use parser::BunnyParser;

fn main() {
    let p = BunnyParser::parse(parser::Rule::program, "1234");
    println!("{:?}", p);
}
