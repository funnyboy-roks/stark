use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, Clone, Parser)]
pub struct Cli {
    #[clap(short, long, conflicts_with = "lex", conflicts_with = "ir")]
    pub parse: bool,
    #[clap(short, long, conflicts_with = "parse", conflicts_with = "ir")]
    pub lex: bool,
    #[clap(short, long, conflicts_with = "lex", conflicts_with = "parse")]
    pub ir: bool,
    #[clap(
        short = 'g',
        long = "gen",
        conflicts_with = "lex",
        conflicts_with = "parse"
    )]
    pub codegen: bool,

    #[clap(long)]
    pub auto_drop: bool,

    #[clap(short = 'o', long = "out")]
    pub asm_out: Option<PathBuf>,
    pub file: PathBuf,
}
