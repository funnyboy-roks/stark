use std::{fs::File, path::Path};

use clap::Parser;
use lex::Lexer;
use miette::NamedSource;

pub mod cli;
pub mod compile;
// pub mod eval;
pub mod hash_float;
pub mod lex;
pub mod parse;

fn main() -> Result<(), miette::Error> {
    let cli = cli::Cli::parse();

    let content = std::fs::read_to_string(&cli.file).unwrap();
    let lex = Lexer::new(&content, cli.file.to_str().unwrap());

    if cli.lex {
        for tok in lex {
            let tok = tok.map_err(|e| {
                miette::Error::from(e).with_source_code(NamedSource::new(
                    cli.file.to_string_lossy(),
                    content.to_string(),
                ))
            })?;
            println!("{:?}", tok);
        }
    } else if cli.parse {
        eprintln!("parsing...");
        let parser = parse::Parser::new(lex);
        let ast = parser.parse().map_err(|e| {
            miette::Error::from(e).with_source_code(NamedSource::new(
                cli.file.to_string_lossy(),
                content.to_string(),
            ))
        })?;
        dbg!(ast);
    } else {
        eprintln!("compiling...");
        let asm_path = cli
            .asm_out
            .as_deref()
            .unwrap_or_else(|| Path::new(cli.file.file_name().unwrap()))
            .with_extension("s");
        let mut asm_file = File::create(asm_path).unwrap();
        let parser = parse::Parser::new(lex);
        let ast = parser.parse().map_err(|e| {
            miette::Error::from(e).with_source_code(NamedSource::new(
                cli.file.to_string_lossy(),
                content.to_string(),
            ))
        })?;
        let comp = compile::Compiler::new(&cli, &ast, &mut asm_file);
        comp.compile().map_err(|e| {
            miette::Error::from(e).with_source_code(NamedSource::new(
                cli.file.to_string_lossy(),
                content.to_string(),
            ))
        })?;
    }
    Ok(())
}
