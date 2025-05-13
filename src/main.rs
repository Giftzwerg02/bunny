pub mod ast;
pub mod cli;
pub mod debug;
pub mod parser;
mod types;
mod runner;
mod library;
mod interpreter;
mod svg;

#[allow(unused)]
use clap::Parser as ClapParser;
#[allow(unused)]
use cli::Cli;

use interpreter::Interpreter;
use miette::Result;
use svg::output_svg;
use crate::library::standard_library;

fn main() -> Result<()> {
    let cli = Cli::parse();

    let mut interpreter = Interpreter::new(standard_library());

    for (name, value) in cli.defined_variables() {
        interpreter.add_predefined_variable(name, value)?;
    }

    let result = interpreter.run_file(cli.file)?;

    if result.is_renderable() {
        output_svg(&result, &cli.render_config);
    }
    else {
        println!("{:?}", result);
    }
    
    Ok(())
}