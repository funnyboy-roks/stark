use std::{error::Error, fs::File, path::Path};

use clap::Parser;
use lexer::Lexer;

pub mod cli;
pub mod compile;
pub mod eval;
pub mod lexer;

fn main() -> Result<(), Box<dyn Error>> {
    let cli = cli::Cli::parse();

    let content = std::fs::read_to_string(&cli.file).unwrap();
    let lex = Lexer::new(&content);

    if cli.lex {
        for tok in lex {
            println!("{:?}", tok);
        }
    } else if cli.interpret {
        eprintln!("evaluating...");
        let eval = eval::Evaluator::default();
        eval.eval(lex.clone())?;
    } else {
        eprintln!("compiling...");
        let comp = compile::Compiler::new(&cli);
        let asm_path = cli
            .asm_out
            .as_deref()
            .unwrap_or_else(|| Path::new(cli.file.file_name().unwrap()))
            .with_extension("s");
        let mut asm_file = File::create(asm_path).unwrap();
        comp.compile(lex, &mut asm_file)?;
    }
    Ok(())
}
