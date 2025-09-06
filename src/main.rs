use std::{fs::File, path::Path};

use clap::Parser;
use lex::Lexer;
use miette::NamedSource;

use crate::ir::Module;

pub mod cli;
pub mod compile;
// pub mod eval;
pub mod codegen;
pub mod hash_float;
pub mod ir;
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
    } else if cli.ir {
        eprintln!("generating ir...");
        let parser = parse::Parser::new(lex);
        let ast = parser.parse().map_err(|e| {
            miette::Error::from(e).with_source_code(NamedSource::new(
                cli.file.to_string_lossy(),
                content.to_string(),
            ))
        })?;
        let mut module = Module::new(ast, false);
        module.compile_module().map_err(|e| {
            miette::Error::from(e).with_source_code(NamedSource::new(
                cli.file.to_string_lossy(),
                content.to_string(),
            ))
        })?;

        eprintln!(
            "[{}:{}:{}] maker.functions = {:?}",
            file!(),
            line!(),
            column!(),
            module.functions,
        );
        eprintln!("Function Signatures:");
        for x in &module.functions {
            eprintln!("    {}", x.1);
        }
        eprintln!("Functions:");
        for x in &module.converted_functions {
            eprintln!("    {}:", x.linker_name);
            for y in &x.body {
                eprintln!("        {:?}", y.kind);
            }
        }
    } else if cli.codegen {
        let parser = parse::Parser::new(lex);
        let ast = parser.parse().map_err(|e| match e {
            parse::ParseError::LexError(e) => miette::Error::from(e).with_source_code(
                NamedSource::new(cli.file.to_string_lossy(), content.to_string()),
            ),
            _ => miette::Error::from(e).with_source_code(NamedSource::new(
                cli.file.to_string_lossy(),
                content.to_string(),
            )),
        })?;
        let mut module = Module::new(ast, true);
        module.compile_module().map_err(|e| {
            miette::Error::from(e).with_source_code(NamedSource::new(
                cli.file.to_string_lossy(),
                content.to_string(),
            ))
        })?;

        eprintln!("generating code...");
        let mut codegen = codegen::CodeGen::new(module, std::io::stdout());
        codegen.compile().map_err(|e| match e {
            codegen::CodeGenError::IrGenError(e) => miette::Error::from(e).with_source_code(
                NamedSource::new(cli.file.to_string_lossy(), content.to_string()),
            ),
            e => miette::Error::from(e).with_source_code(NamedSource::new(
                cli.file.to_string_lossy(),
                content.to_string(),
            )),
        })?;
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
