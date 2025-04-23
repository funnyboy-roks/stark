use std::{collections::HashMap, error::Error, fmt::Display, io::Write, iter::Peekable};

use crate::{
    cli::Cli,
    lexer::{LexError, Token, TokenKind},
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Type {
    // TODO: I8, I16, ..
    // TODO: U8, U16, ..
    I64,
    // TODO: typed pointers
    Pointer,
    // TODO: make this work
    FatPointer,
}

impl Type {
    fn add(self, lhs: Self) -> Result<Type, CompileError> {
        match (self, lhs) {
            (Type::I64, Type::I64) => Ok(Type::I64),
            (Type::I64, Type::Pointer) => Ok(Type::Pointer),
            (Type::Pointer, Type::I64) => Ok(Type::Pointer),
            (Type::Pointer, Type::Pointer) => {
                Err(CompileError::TypeError("Cannot add pointer to pointer"))
            }
            _ => Err(CompileError::TypeError("TODO: Type errors")),
        }
    }

    fn from_ident(ident: &str) -> Option<Type> {
        match ident {
            "i64" => Some(Self::I64),
            "ptr" => Some(Self::Pointer),
            _ => None,
        }
    }

    pub fn size(self) -> u32 {
        match self {
            Type::I64 => 8,
            Type::Pointer => 8,
            Type::FatPointer => 8 + 8,
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
    UnexpectedToken,
    UnexpectedEof,
    RepeatDefinition,
    IncorrectExplicitArgSize {
        expected: usize,
        actual: usize,
        variadic: bool,
    },
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternFunction {
    linker_name: String,
    // tok_{start,end} point to the ident
    tok_start: usize,
    tok_end: usize,
    args: Vec<Type>,
    variadic: bool,
    returns: Vec<Type>,
}

pub struct Compiler<'a> {
    cli: &'a Cli,
    type_stack: Vec<Type>,
    data: HashMap<Box<[u8]>, usize>,
    extern_functions: HashMap<String, ExternFunction>,
}

impl<'a> Compiler<'a> {
    pub fn new(cli: &'a Cli) -> Self {
        Self {
            cli,
            type_stack: Default::default(),
            data: Default::default(),
            extern_functions: Default::default(),
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

    fn expect_token<'b>(
        &mut self,
        tokens: &mut impl Iterator<Item = Result<Token<'b>, LexError>>,
        expected: TokenKind<'_>,
    ) -> Result<(), CompileError> {
        let tok = tokens.next().ok_or(CompileError::UnexpectedEof)??;
        if tok.kind != expected {
            return Err(CompileError::UnexpectedToken);
        }
        Ok(())
    }

    fn take_args<'b>(
        &mut self,
        tokens: &mut impl Iterator<Item = Result<Token<'b>, LexError>>,
    ) -> Result<(Vec<Type>, bool), CompileError> {
        let mut args = Vec::new();
        let mut variadic = false;
        for t in tokens {
            let t = t?;
            match t.kind {
                TokenKind::Ident(cow) if !variadic => {
                    let Some(ty) = Type::from_ident(&cow) else {
                        dbg!(cow);
                        return Err(CompileError::UnexpectedToken);
                    };
                    args.push(ty);
                }
                TokenKind::RParen => {
                    break;
                }
                TokenKind::Ellipsis if !variadic => {
                    variadic = true;
                }
                _ => {
                    dbg!(t);
                    return Err(CompileError::UnexpectedToken);
                }
            }
        }
        Ok((args, variadic))
    }

    // TODO: dedicated parsing step
    /// ```
    /// extern fn strlen(ptr) -> (i64);
    /// ```
    fn parse_extern_fn<'b>(
        &mut self,
        tokens: &mut impl Iterator<Item = Result<Token<'b>, LexError>>,
    ) -> Result<String, CompileError> {
        self.expect_token(tokens, TokenKind::Fn)?;
        let ident_tok = tokens.next().ok_or(CompileError::UnexpectedEof)??;
        let TokenKind::Ident(ident) = ident_tok.kind else {
            return Err(CompileError::UnexpectedToken);
        };
        self.expect_token(tokens, TokenKind::LParen)?;

        let (args, variadic) = self.take_args(tokens)?;
        self.expect_token(tokens, TokenKind::Arrow)?;
        self.expect_token(tokens, TokenKind::LParen)?;
        let (returns, false) = self.take_args(tokens)? else {
            return Err(CompileError::UnexpectedToken);
        };
        self.expect_token(tokens, TokenKind::Semicolon)?;
        let new_fn = ExternFunction {
            linker_name: ident.to_string(),
            tok_start: ident_tok.offset,
            tok_end: ident_tok.end,
            args,
            variadic,
            returns,
        };
        let existing = self.extern_functions.insert(ident.to_string(), new_fn);

        if existing.is_some() {
            return Err(CompileError::RepeatDefinition);
        }

        Ok(ident.to_string())
    }

    fn get_call_len<'b>(
        &mut self,
        tokens: &mut Peekable<impl Iterator<Item = Result<Token<'b>, LexError>>>,
    ) -> Result<Option<u32>, CompileError> {
        let Some(tok) = tokens.peek() else {
            return Ok(None);
        };
        if tok.as_ref().is_ok_and(|t| t.kind != TokenKind::LParen) {
            return Ok(None);
        }
        tokens.next().expect("checked above with .peek")?;

        let Some(tok) = tokens.peek() else {
            return Ok(None);
        };
        let num = match tok {
            Ok(tok) => {
                let TokenKind::IntLit(num) = tok.kind else {
                    return Ok(None);
                };
                tokens.next().expect("checked above with .peek")?;
                num
            }
            Err(_) => {
                return Ok(None);
            }
        };

        let Some(tok) = tokens.peek() else {
            return Ok(None);
        };
        if tok.as_ref().is_ok_and(|t| t.kind != TokenKind::RParen) {
            return Ok(None);
        };
        tokens.next().expect("checked above with .peek")?;

        Ok(Some(num as _))
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
        writeln!(output, r#"public main"#)?;
        writeln!(output, r#"main:"#)?;

        while let Some(t) = tokens.next() {
            let t = t?;
            match t.kind {
                TokenKind::Ident(ref s) => {
                    //
                    match &**s {
                        "dump_stack" => {
                            dbg!(&self.type_stack);
                        }
                        "syscall3" => {
                            dbg!(&self.type_stack);

                            self.pop(&t)?;
                            writeln!(output, "    popq rdx")?;

                            self.pop(&t)?;
                            writeln!(output, "    popq rsi")?;

                            self.pop(&t)?;
                            writeln!(output, "    popq rdi")?;

                            self.pop_type(&t, Type::I64)?;
                            writeln!(output, "    popq rax")?;

                            writeln!(output, "    syscall")?;

                            self.type_stack.push(Type::I64);
                            writeln!(output, "    pushq rax")?;
                        }
                        // "exit" => {
                        //     dbg!(&self.type_stack);
                        //     self.pop_type(&t, Type::I64)?;
                        //     writeln!(output, "    popq rdi")?;
                        //     writeln!(output, "    mov rax, 60")?;
                        //     writeln!(output, "    syscall")?;
                        // }
                        s => {
                            let Some(ext) = self.extern_functions.get(s) else {
                                todo!("ident '{}'", s);
                            };
                            // TODO
                            let ext = ext.clone();

                            let len = self
                                .get_call_len(&mut tokens)?
                                .unwrap_or(ext.args.len() as u32);
                            let registers = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

                            if (ext.variadic && (len as usize) < ext.args.len())
                                || (!ext.variadic && ext.args.len() != len as usize)
                            {
                                return Err(CompileError::IncorrectExplicitArgSize {
                                    expected: ext.args.len(),
                                    actual: len as usize,
                                    variadic: ext.variadic,
                                });
                            }

                            for i in 0..len as usize {
                                if let Some(r) = registers.get(i) {
                                    writeln!(output, "    popq {}", r)?;
                                }

                                if i < ext.args.len() {
                                    self.pop_type(&t, ext.args[i])?;
                                } else {
                                    self.pop(&t)?;
                                }
                            }

                            writeln!(output, "    mov rax, 0")?;
                            writeln!(output, "    call {}", ext.linker_name)?;
                            for i in 0..len as usize {
                                // Pop _our_ arguments from the stack (this is kinda sus, ngl)
                                // TODO: validate this is correct
                                if i >= registers.len() {
                                    writeln!(output, "    popq rdi")?;
                                }
                            }
                            if !ext.returns.is_empty() {
                                writeln!(output, "    pushq rax")?;
                            }

                            for t in ext.returns {
                                self.type_stack.push(t);
                            }
                        }
                    }
                }
                // TODO: string fat pointers
                TokenKind::StrLit(s) => {
                    let n = if let Some(n) = self.data.get(s.as_bytes()) {
                        *n
                    } else {
                        let next = self.data.len();
                        let mut s = s.into_owned().into_bytes();
                        s.push(0);
                        self.data.insert(s.into_boxed_slice(), next);
                        next
                    };
                    self.type_stack.push(Type::FatPointer);
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
                    self.type_stack.push(Type::I64);
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
                TokenKind::Extern => {
                    let name = self.parse_extern_fn(&mut tokens)?;
                    writeln!(output, "extrn {}", name)?;
                }
                kind => todo!("Token kind: {:?}", kind),
            }
            writeln!(output)?;
        }

        if !self.type_stack.is_empty() {
            if self.cli.auto_drop {
                eprintln!(
                    "[WARN] {} {} still left on stack: {:?}",
                    self.type_stack.len(),
                    if self.type_stack.len() == 1 {
                        "item"
                    } else {
                        "items"
                    },
                    &self.type_stack
                );
                for _ in &self.type_stack {
                    writeln!(output, "    popq rax")?;
                }
            } else {
                return Err(CompileError::StackNotEmpty);
            }
        }

        writeln!(output, "    mov rax, 0")?;
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

        Ok(())
    }
}
