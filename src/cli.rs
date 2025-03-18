use std::path::PathBuf;

use clap::{Parser, command};

#[command(author, version, about, long_about = None)]
#[derive(Parser, Debug)]
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