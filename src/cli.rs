use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, Clone, Parser)]
pub struct Cli {
    #[clap(short, long)]
    pub parse: bool,
    #[clap(short, long, conflicts_with = "parse")]
    pub lex: bool,

    #[clap(long)]
    pub auto_drop: bool,

    #[clap(short = 'o', long = "out")]
    pub asm_out: Option<PathBuf>,
    pub file: PathBuf,
}
