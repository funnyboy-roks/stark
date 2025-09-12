use std::{
    fs::File,
    io::{BufReader, Cursor},
    path::Path,
};

use clap::Parser;
use lex::Lexer;
use miette::{highlighters::SyntectHighlighter, NamedSource};
use syntect::highlighting::ThemeSet;

use crate::{cli::Cli, ir::Module};

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

    miette::set_hook(Box::new(|_| {
        // TODO: custom syntax, but syntect is a PITA
        let mut a = include_bytes!("../theme/base16-circus.tmTheme").as_slice();
        let mut buf = BufReader::new(Cursor::new(&mut a));
        Box::new(
            miette::MietteHandlerOpts::new()
                .terminal_links(true)
                .with_syntax_highlighting(SyntectHighlighter::new_themed(
                    ThemeSet::load_from_reader(&mut buf).unwrap(),
                    false,
                ))
                .context_lines(3)
                .build(),
        )
    }))
    .unwrap();

    fn wrap_miette<T>(
        r: Result<T, miette::Error>,
        cli: &Cli,
        content: impl Into<String>,
    ) -> Result<T, miette::Report> {
        r.map_err(|e| {
            e.with_source_code(
                NamedSource::new(cli.file.to_string_lossy(), content.into()).with_language("Rust"),
            )
        })
    }

    wrap_miette(main2(&cli, content.clone()), &cli, content)?;

    Ok(())
}

fn main2(cli: &Cli, content: String) -> Result<(), miette::Error> {
    let lex = Lexer::new(&content, cli.file.to_str().unwrap());
    if cli.subcmd.lex {
        for tok in lex {
            let tok = tok?;
            println!("{:?}", tok);
        }
    } else if cli.subcmd.parse {
        eprintln!("parsing...");
        let parser = parse::Parser::new(lex);
        let ast = parser.parse()?;
        dbg!(ast);
    } else if cli.subcmd.ir {
        eprintln!("generating ir...");
        let parser = parse::Parser::new(lex);
        let ast = parser.parse()?;
        let mut module = Module::new(ast, false);
        module.compile_module()?;

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
    } else if cli.subcmd.old {
        eprintln!("compiling...");
        let asm_path = cli
            .asm_out
            .as_deref()
            .unwrap_or_else(|| Path::new(cli.file.file_name().unwrap()))
            .with_extension("s");
        let mut asm_file = File::create(asm_path).unwrap();
        let parser = parse::Parser::new(lex);
        let ast = parser.parse()?;
        let comp = compile::Compiler::new(cli, &ast, &mut asm_file);
        comp.compile()?;
    } else {
        let parser = parse::Parser::new(lex);
        let ast = parser.parse()?;
        let mut module = Module::new(ast, true);
        module.compile_module()?;

        let out_path = cli
            .asm_out
            .as_deref()
            .unwrap_or_else(|| Path::new(cli.file.file_name().unwrap()))
            .with_extension("s");

        let mut out = File::create(out_path).unwrap();

        eprintln!("generating code...");
        let mut codegen = codegen::CodeGen::new(module, &mut out);
        codegen.compile()?;
    }
    Ok(())
}
