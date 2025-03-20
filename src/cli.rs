use std::path::PathBuf;

use clap::{Parser, command};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Activate debug mode
    #[arg(long)]
    pub debug: bool,

    /// The input source file
    pub file: PathBuf,

    /// Custom arguments passed to the program
    #[arg(last = true)]
    pub custom_args: Vec<String>,
}
