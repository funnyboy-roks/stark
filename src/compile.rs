use std::{collections::HashMap, error::Error, fmt::Display, io::Write};

use crate::{
    cli::Cli,
    lexer::{LexError, Token, TokenKind},
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Type {
    Int,
    Pointer,
    // FatPointer,
}

impl Type {
    fn add(self, lhs: Self) -> Result<Type, CompileError> {
        match (self, lhs) {
            (Type::Int, Type::Int) => Ok(Type::Int),
            (Type::Int, Type::Pointer) => Ok(Type::Pointer),
            (Type::Pointer, Type::Int) => Ok(Type::Pointer),
            (Type::Pointer, Type::Pointer) => {
                Err(CompileError::TypeError("Cannot add pointer to pointer"))
            }
        }
    }
}

#[derive(Debug)]
pub enum CompileError {
    LexError(LexError),
    IoError(std::io::Error),
    StackUnderflow {
        tok_start: usize,
        tok_end: usize,
    },
    TypeError(&'static str),
    TypeError2 {
        tok_start: usize,
        tok_end: usize,
        message: String,
    },
    StackNotEmpty,
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<std::io::Error> for CompileError {
    fn from(value: std::io::Error) -> Self {
        Self::IoError(value)
    }
}

impl From<LexError> for CompileError {
    fn from(value: LexError) -> Self {
        Self::LexError(value)
    }
}

impl Error for CompileError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }

    fn description(&self) -> &str {
        "description() is deprecated; use Display"
    }

    fn cause(&self) -> Option<&dyn Error> {
        self.source()
    }
}

pub struct Compiler<'a> {
    cli: &'a Cli,
    type_stack: Vec<Type>,
    data: HashMap<Box<[u8]>, usize>,
}

impl<'a> Compiler<'a> {
    pub fn new(cli: &'a Cli) -> Self {
        Self {
            cli,
            type_stack: Default::default(),
            data: Default::default(),
        }
    }

    fn pop(&mut self, token: &Token<'_>) -> Result<Type, CompileError> {
        self.type_stack.pop().ok_or(CompileError::StackUnderflow {
            tok_start: token.offset,
            tok_end: token.end,
        })
    }

    fn pop_type(&mut self, token: &Token<'_>, expected: Type) -> Result<(), CompileError> {
        let ty = self.pop(token)?;
        if ty != expected {
            return Err(CompileError::TypeError2 {
                tok_start: token.offset,
                tok_end: token.end,
                message: format!(
                    "Type error: expected {:?} on stack, found {:?}",
                    expected, ty
                ),
            });
        }
        Ok(())
    }

    pub fn compile<'b>(
        mut self,
        tokens: impl Iterator<Item = Result<Token<'b>, LexError>>,
        output: &mut impl Write,
    ) -> Result<(), CompileError> {
        let mut tokens = tokens.peekable();
        writeln!(output, r#"format ELF64"#)?;
        writeln!(output, r#"section ".text" executable"#)?;
        writeln!(output)?;
        writeln!(output, r#"extrn printf"#)?;
        writeln!(output, r#"public main"#)?;
        writeln!(output, r#"main:"#)?;

        while let Some(t) = tokens.next() {
            let t = t?;
            match t.kind {
                TokenKind::Ident(ref s) => {
                    //
                    match &**s {
                        "syscall3" => {
                            dbg!(&self.type_stack);

                            self.pop(&t)?;
                            writeln!(output, "    popq rdx")?;

                            self.pop(&t)?;
                            writeln!(output, "    popq rsi")?;

                            self.pop(&t)?;
                            writeln!(output, "    popq rdi")?;

                            self.pop_type(&t, Type::Int)?;
                            writeln!(output, "    popq rax")?;

                            writeln!(output, "    syscall")?;

                            self.type_stack.push(Type::Int);
                            writeln!(output, "    pushq rax")?;
                        }
                        "printf" => {
                            let args = 't: {
                                let Some(tok) = tokens.peek() else {
                                    break 't 0;
                                };
                                if tok.as_ref().is_ok_and(|t| t.kind != TokenKind::LParen) {
                                    break 't 0;
                                }
                                tokens.next().expect("checked above with .peek")?;

                                let Some(tok) = tokens.peek() else {
                                    break 't 0;
                                };
                                let num = match tok {
                                    Ok(tok) => {
                                        let TokenKind::IntLit(num) = tok.kind else {
                                            break 't 0;
                                        };
                                        tokens.next().expect("checked above with .peek")?;
                                        num
                                    }
                                    Err(_) => {
                                        break 't 0;
                                    }
                                };

                                let Some(tok) = tokens.peek() else {
                                    break 't 0;
                                };
                                if tok.as_ref().is_ok_and(|t| t.kind != TokenKind::RParen) {
                                    break 't 0;
                                };
                                tokens.next().expect("checked above with .peek")?;

                                num
                            };
                            writeln!(output, "    popq rdi")?;
                            let _ = self.pop(&t)?;
                            if args >= 1 {
                                writeln!(output, "    popq rsi")?;
                            }
                            for _ in 0..args {
                                let _ = self.pop(&t)?;
                            }
                            writeln!(output, "    mov eax, 0")?;
                            writeln!(output, "    call printf")?;
                        }
                        "exit" => {
                            dbg!(&self.type_stack);
                            self.pop_type(&t, Type::Int)?;
                            writeln!(output, "    popq rdi")?;
                            writeln!(output, "    mov rax, 60")?;
                            writeln!(output, "    syscall")?;
                        }
                        _ => {
                            todo!("ident '{}'", s);
                        }
                    }
                }
                // TODO: string fat pointers
                TokenKind::StrLit(s) => {
                    let n = if let Some(n) = self.data.get(s.as_bytes()) {
                        *n
                    } else {
                        let next = self.data.len();
                        let s = s.into_owned().into_bytes();
                        self.data.insert(s.into_boxed_slice(), next);
                        next
                    };
                    self.type_stack.push(Type::Pointer);
                    writeln!(output, "    pushq string_{}", n)?;
                }
                TokenKind::CStrLit(s) => {
                    let n = if let Some(n) = self.data.get(s.as_bytes()) {
                        *n
                    } else {
                        let next = self.data.len();
                        let mut s = s.into_owned().into_bytes();
                        s.push(0);
                        self.data.insert(s.into_boxed_slice(), next);
                        next
                    };
                    self.type_stack.push(Type::Pointer);
                    writeln!(output, "    pushq string_{}", n)?;
                }
                TokenKind::IntLit(n) => {
                    writeln!(output, "    pushq {}", n)?;
                    self.type_stack.push(Type::Int);
                }
                TokenKind::Plus => {
                    let x = self.pop(&t)?;
                    writeln!(output, "    popq rax")?;
                    let y = self.pop(&t)?;
                    writeln!(output, "    popq rbx")?;
                    writeln!(output, "    add rax, rbx")?;
                    self.type_stack.push(x.add(y)?);
                    writeln!(output, "    push rax")?;
                }
                TokenKind::Minus => {
                    let x = self.pop(&t)?;
                    let y = self.pop(&t)?;
                    self.type_stack.push(x.add(y)?);
                    writeln!(output, "    popq rax")?;
                    writeln!(output, "    popq rbx")?;
                }
                TokenKind::Asterisk => todo!("asterisk"),
                TokenKind::Slash => todo!("slash"),
                TokenKind::Dup => {
                    writeln!(output, "    popq rax")?;
                    writeln!(output, "    pushq rax")?;
                    writeln!(output, "    pushq rax")?;
                    let x = self.pop(&t)?;
                    self.type_stack.push(x);
                    self.type_stack.push(x);
                }
                TokenKind::Drop => {
                    writeln!(output, "    popq rax")?;
                    let _ = self.pop(&t)?;
                }
                TokenKind::LParen | TokenKind::RParen => unreachable!(),
                kind => eprintln!("[NYI] Token kind: {:?}", kind),
            }
            writeln!(output)?;
        }

        writeln!(output, "    ret")?;

        for (s, n) in self.data {
            writeln!(output, "string_{}:", n)?;
            write!(output, "    db ")?;
            for (i, c) in s.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ")?;
                }
                write!(output, "{}", c)?;
            }
            writeln!(output)?;
        }

        if !self.type_stack.is_empty() {
            return Err(CompileError::StackNotEmpty);
        }

        Ok(())
    }
}
