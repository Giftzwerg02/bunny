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

    let interpreter = Interpreter::new(standard_library());

    // TODO God that's awful, but one impossible thing at a time
    let eternal_interpreter = Box::leak(Box::new(interpreter)); 

    let result = eternal_interpreter.run_file(cli.file)?;

    if result.is_renderable() {
        output_svg(&result, &cli.render_config);
    }
    
    Ok(())
}