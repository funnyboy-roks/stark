use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, Clone, Parser)]
pub struct Cli {
    #[clap(short, long)]
    pub interpret: bool,
    #[clap(short, long, conflicts_with = "interpret")]
    pub lex: bool,

    #[clap(short = 'p', long = "out")]
    pub asm_out: Option<PathBuf>,
    pub file: PathBuf,
}
