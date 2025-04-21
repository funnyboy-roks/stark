use std::error::Error;

use lexer::Lexer;

pub mod eval;
pub mod lexer;

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args();
    let program = args.next().unwrap();
    let Some(file) = args.next() else {
        eprintln!("{} <file>.st", program);
        return Ok(());
    };
    let content = std::fs::read_to_string(file).unwrap();
    let lex = Lexer::new(&content);

    let eval = eval::Evaluator::new(lex);
    eval.eval()?;
    Ok(())
}
