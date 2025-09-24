use std::path::PathBuf;

use clap::{Args, Parser};

#[derive(Debug, Clone, Args)]
#[group(multiple = false)]
pub struct SubCmd {
    #[clap(short, long)]
    pub parse: bool,
    #[clap(short, long)]
    pub lex: bool,
    #[clap(short, long)]
    pub ir: bool,
}

#[derive(Debug, Clone, Parser)]
pub struct Cli {
    #[command(flatten)]
    pub subcmd: SubCmd,

    #[clap(long)]
    pub auto_drop: bool,

    #[clap(short = 'o', long = "out")]
    pub asm_out: Option<PathBuf>,

    pub file: PathBuf,
}
