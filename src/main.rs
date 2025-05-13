pub mod ast;
pub mod cli;
pub mod debug;
pub mod parser;
mod types;
mod runner;
mod library;
mod interpreter;

use std::{fs::{self}, io::Write};

#[allow(unused)]
use clap::Parser as ClapParser;
#[allow(unused)]
use cli::Cli;
use esvg::{create_document, page::Page};
use interpreter::Interpreter;
use miette::Result;
use runner::value::Value;
use crate::library::standard_library;

fn main() -> Result<()> {
    let lib = standard_library();
    let mut interpreter = Interpreter::new(lib);

    let result = interpreter.run_file("examples/duplicate-names.bny")?;

    let svg = render_page(result);
    let mut file = fs::File::create("out.svg").unwrap();
    file.write_all(svg.as_bytes()).unwrap();

    Ok(())
}

fn render_page(val: Value) -> String {
    let Value::Opaque(child) = val else { panic!() };

    let page = Page::A4(96);  // 96 dpi
    let mut doc = create_document(&page);
    doc.add(&child);
    doc.to_pretty_string()
}
