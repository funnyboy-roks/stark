#![allow(clippy::len_zero)]

use std::{
    fs::File,
    io::{BufReader, Cursor},
    path::Path,
    rc::Rc,
    sync::Arc,
};

use clap::Parser;
use lex::Lexer;
use miette::{Context, IntoDiagnostic, NamedSource, highlighters::SyntectHighlighter};
use syntect::highlighting::ThemeSet;

use crate::{cli::Cli, ir::Module};

pub mod cli;
// pub mod eval;
pub mod codegen;
pub mod hash_float;
pub mod ir;
pub mod lex;
pub mod parse;
pub mod span;

fn main() -> Result<(), miette::Error> {
    let cli = cli::Cli::parse();

    let mut content = std::fs::read_to_string(&cli.file).unwrap();
    // trim in-place
    while content.ends_with(char::is_whitespace) {
        content.pop();
    }

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

    let mut errors = Vec::new();
    if main2(&cli, content.clone(), &mut errors).is_err() {
        let content = Arc::new(content);
        for e in errors {
            let src = NamedSource::new(cli.file.to_string_lossy(), Arc::clone(&content))
                .with_language("Rust");
            let e = e.with_source_code(src);
            eprintln!("{:?}", e);
        }
    }

    Ok(())
}

fn main2(cli: &Cli, content: String, errors: &mut Vec<miette::Error>) -> Result<(), ()> {
    let lex = Lexer::new(&content, cli.file.to_str().unwrap());
    if cli.subcmd.lex {
        let content = Arc::new(content.clone());
        for tok in lex {
            match tok {
                Ok(tok) => {
                    println!("{:?}", tok);
                }
                Err(e) => {
                    let src = NamedSource::new(cli.file.to_string_lossy(), Arc::clone(&content))
                        .with_language("Rust");
                    let e = miette::Error::from(e).with_source_code(src);
                    println!("[Error] {:?}", e)
                }
            }
        }
    } else if cli.subcmd.parse {
        eprintln!("parsing...");
        let parser = parse::Parser::new(lex);
        let ast = match parser
            .parse_module()
            .map_err(|e| e.into_iter().map(miette::Error::from).collect::<Vec<_>>())
        {
            Ok(ast) => ast,
            Err(e) => {
                errors.extend(e);
                return Err(());
            }
        };
        dbg!(ast);
    } else if cli.subcmd.ir {
        eprintln!("generating ir...");
        let parser = parse::Parser::new(lex);
        let ast = match parser
            .parse_module()
            .map_err(|e| e.into_iter().map(miette::Error::from).collect::<Vec<_>>())
        {
            Ok(ast) => ast,
            Err(e) => {
                errors.extend(e);
                return Err(());
            }
        };
        let mut module = Module::new(
            ast,
            vec![
                cli.file
                    .with_extension("")
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .to_string(),
            ],
            cli.file.clone(),
            content,
            false,
        );
        let mut ir_errors = Vec::new();
        if module.scan_functions(&mut ir_errors).is_ok() {
            module.update_roots(Rc::new(module.light_clone()));
            let _ = module.compile_module(&mut ir_errors);
        }
        if !ir_errors.is_empty() {
            ir_errors
                .into_iter()
                .map(miette::Error::from)
                .for_each(|e| errors.push(e));
            return Err(());
        }
        fn print_module(module: &Module, indent: usize) {
            eprintln!("{1:>0$}Imports:", indent * 4, "");
            for (from, to) in &module.imports {
                eprintln!("{1:>0$}    {2:?} => {3:?}", indent * 4, "", from, to);
            }
            eprintln!("{1:>0$}Function Signatures:", indent * 4, "");
            for x in &module.functions {
                eprintln!("{1:>0$}    {2}", indent * 4, "", x.1);
            }
            eprintln!("{1:>0$}Functions:", indent * 4, "");
            for x in &module.converted_functions {
                eprintln!("{1:>0$}    {2}:", indent * 4, "", x.linker_name);
                for y in &x.body {
                    eprintln!("{1:>0$}        {2:?}", indent * 4, "", y.kind);
                }
            }
            eprintln!("{1:>0$}Submodules:", indent * 4, "");
            for (name, (module, vis)) in &module.submodules {
                eprintln!("{1:>0$}    {vis:?} mod {name}:", indent * 4, "");
                print_module(module, indent + 2);
            }
        }

        print_module(&module, 1);
    } else {
        let parser = parse::Parser::new(lex);
        let ast = match parser
            .parse_module()
            .map_err(|e| e.into_iter().map(miette::Error::from).collect::<Vec<_>>())
        {
            Ok(ast) => ast,
            Err(e) => {
                errors.extend(e);
                return Err(());
            }
        };
        let mut module = Module::new(
            ast,
            vec![
                cli.file
                    .with_extension("")
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .to_string(),
            ],
            cli.file.clone(),
            content,
            true,
        );

        let mut ir_errors = Vec::new();
        if module.scan_functions(&mut ir_errors).is_ok() {
            module.update_roots(Rc::new(module.light_clone()));
            let _ = module.compile_module(&mut ir_errors);
        }
        if !ir_errors.is_empty() {
            ir_errors
                .into_iter()
                .map(miette::Error::from)
                .for_each(|e| errors.push(e));
            return Err(());
        }

        let out_path = cli
            .asm_out
            .as_deref()
            .unwrap_or_else(|| Path::new(cli.file.file_name().unwrap()))
            .with_extension("s");

        let mut out = match File::create(out_path) {
            Ok(out) => out,
            Err(e) => {
                errors.push(
                    Err::<(), _>(e)
                        .into_diagnostic()
                        .context("Unable to create output file")
                        .unwrap_err(),
                );
                return Err(());
            }
        };

        eprintln!("generating code...");
        let mut codegen = codegen::CodeGen::new(module, &mut out);
        if let Err(e) = codegen.compile() {
            errors.push(miette::Error::from(e));
            return Err(());
        }
    }
    Ok(())
}
