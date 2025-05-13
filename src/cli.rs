use std::path::PathBuf;

use clap::{command, Args, Parser};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Activate debug mode
    #[arg(long)]
    pub debug: bool,

    /// The input source file
    pub file: PathBuf,

    #[clap(flatten)]
    pub render_config: RenderConfig,

    /// Custom arguments passed to the program
    #[arg(last = true)]
    pub custom_args: Vec<String>,
}


#[derive(Debug, Args)]
pub struct RenderConfig {
    /// The output for the generated SVG
    pub output_file: Option<PathBuf>,

    /// The output for the generated PNG
    #[arg(long, default_value_t = 256)]
    pub width: i32,

    /// The height of the canvas
    #[arg(long, default_value_t = 256)]
    pub height: i32,

    #[arg(long, default_value_t = 96)]
    /// The DPI of the canvas
    pub dpi: i32,
    
    /// The margin of the canvas
    #[arg(long, default_value_t = 0.0)]
    pub margin: f64,
}