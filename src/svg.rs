use core::panic;
use std::{fs, io::Write};

use crate::{cli::RenderConfig, runner::value::Value};
use esvg::{create_document, page::Page};
use miette::{miette, Result};

pub fn output_svg(val: &Value, config: &RenderConfig) -> Result<()> {
    let Value::Opaque(child) = val else { panic!("Expected opaque value") };

    let page = Page::build_page(
        &format!("{}x{}", config.width, config.height),
        config.dpi, 
        config.margin
    )
    .map_err(|err| miette!("Invalid page format: {err}"))?;

    let mut doc = create_document(&page);
    doc.add(child);

    let svg = doc.to_pretty_string();

    if let Some(output_file) = &config.output {
        let mut file = fs::File::create(output_file)
            .map_err(|err| miette!("Error creating file: {err}"))?;

        file.write_all(svg.as_bytes())
            .map_err(|err| miette!("Error writing to file: {err}"))?;
    }
    else {
        println!("{svg}")
    }

    Ok(())
}
