use std::{fs, io::Write};

use crate::{cli::RenderConfig, runner::value::Value};
use esvg::{create_document, page::Page};

pub fn save_svg(val: &Value, config: &RenderConfig){
    let Value::Opaque(child) = val else { todo!() };

    let page = Page::build_page(
        &format!("{}x{}", config.width, config.height),
        config.dpi, 
        config.margin
    ).unwrap(); // TODO Error handling


    let mut doc = create_document(&page);
    doc.add(&child);
    doc.to_pretty_string();

    let svg = doc.to_pretty_string();

    let mut file = fs::File::create("out.svg").unwrap(); // TODO Error handling
    file.write_all(svg.as_bytes()).unwrap(); // TODO Error handling
}