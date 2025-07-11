use std::path::PathBuf;

use clap::{command, Args, Parser};

use miette::Result;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Activate debug mode
    #[arg(long)]
    pub debug: bool,

    /// The input source file
    pub file: Option<PathBuf>,

    #[clap(flatten)]
    pub render_config: RenderConfig,

    /// Custom arguments passed to the program
    #[arg(last = true)]
    pub custom_args: Vec<String>,
}

impl Cli {
    pub fn defined_variables(&self) -> Result<Vec<(String, String)>> {
        self.custom_args
            .chunks_exact(2)
            .map(|chunk| {
                if chunk.len() == 2 && chunk[0].starts_with("--") {
                    let key = chunk[0].trim_start_matches("--").to_string();
                    let value = chunk[1].to_string();
                    Ok((key, value))
                } else {
                    Err(miette::miette!("Invalid argument format"))
                }
            })
            .collect()
    }
}

#[derive(Debug, Args)]
pub struct RenderConfig {
    /// The output for the generated SVG
    #[arg(long)]
    pub output: Option<PathBuf>,

    /// NOTE: The width and height works in correlation with the DPI
    ///       !A width of 10 does *not* mean that a point with x=11 does not get drawn!
    ///       Rather: Given a DPI of 96 and a width of 10, the range of drawn points along
    ///       the x-axis is [0; DPI * width = 960]
    /// The output for the generated PNG
    #[arg(long, default_value_t = 5)]
    pub width: i32,

    /// The height of the canvas
    #[arg(long, default_value_t = 5)]
    pub height: i32,

    #[arg(long, default_value_t = 96)]
    /// The DPI of the canvas
    pub dpi: i32,
    
    /// The margin of the canvas
    #[arg(long, default_value_t = 0.0)]
    pub margin: f64,
}
