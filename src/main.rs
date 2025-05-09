pub mod ast;
pub mod cli;
pub mod debug;
pub mod parser;
mod types;
mod runner;
mod library;

use std::{fs::{self}, io::Write};

use ast::{
    PrettyPrintable,
    parsed::{is_not_comment, parsed_expr_pass},
    scoped::scoped_expr_pass,
};
#[allow(unused)]
use clap::Parser as ClapParser;
#[allow(unused)]
use cli::Cli;
use esvg::{create_document, page::Page};
use miette::{NamedSource, Result};
use parser::{BunnyParser, Rule};
use pest::Parser;
use runner::{value::{Lazy, Value}, Runner};
use types::typed::TypedStageInfo;
use crate::ast::Symbol;
use crate::library::standard_library;
use crate::types::{typecheck_pass, InferenceState};
use crate::types::typed::{PolyTypedStageInfo, TypedValue};


fn main() -> Result<()> {
    let input = fs::read_to_string("src/parser/examples/duplicate-names.bny").unwrap();

    let source = NamedSource::new("src/parser/examples/duplicate-names.bny", input.clone())
        .with_language("lisp");

    // TODO Use the pest-miette interop
    let mut pair = BunnyParser::parse(Rule::program, input.leak()).unwrap().filter(is_not_comment);
    let pair = pair.next().expect("no program :(");
    let ast = parsed_expr_pass(pair.clone());

    let mut std_library = standard_library();

    let ast = scoped_expr_pass(ast, &std_library.scoped);
    //println!("{}", ast.pretty_print());

    let typ = typecheck_pass(&ast, &mut std_library.typed)
        .map_err(|report|{
            report.with_source_code(source)
        })?;

    println!("{}", typ.pretty_print());

    let typ = typ.map_stage(
        &mut |typed_info: TypedStageInfo| typed_info.generalize(&std_library.typed.hm)
    );

    let mut runner = Runner::new();
    let result = runner.run(typ, std_library.runnable);
    let evalled = result.eval();
    println!("result: {:?}", &evalled);

    //println!("{}", render_page(result.eval()));

    let svg = render_page(evalled);
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
