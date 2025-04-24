use std::{collections::HashMap, fmt::Display, io::Write};

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::{
    cli::Cli,
    lex::LexError,
    parse::{Ast, Atom, AtomKind, ExternFn, Ident, While},
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
    Bool,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I64 => write!(f, "i64"),
            Type::Pointer => write!(f, "ptr"),
            Type::FatPointer => write!(f, "fatptr"),
            Type::Bool => write!(f, "bool"),
        }
    }
}

impl Type {
    fn add(self, lhs: Self, span: SourceSpan) -> Result<Type, CompileError> {
        match (self, lhs) {
            (Type::I64, Type::I64) => Ok(Type::I64),
            (Type::I64, Type::Pointer) => Ok(Type::Pointer),
            (Type::Pointer, Type::I64) => Ok(Type::Pointer),
            (Type::Pointer, Type::Pointer) => Err(CompileError::TypeError2 {
                message: "Cannot add pointer to pointer".into(),
                span,
            }),
            _ => Err(CompileError::TypeError2 {
                message: "TODO: Type errors".into(),
                span,
            }),
        }
    }

    fn sub(self, lhs: Self, span: SourceSpan) -> Result<Type, CompileError> {
        match (self, lhs) {
            (Type::I64, Type::I64) => Ok(Type::I64),
            (Type::Pointer, Type::I64) => Ok(Type::Pointer),
            _ => Err(CompileError::TypeError2 {
                message: "TODO: Type errors".into(),
                span,
            }),
        }
    }

    fn equals(self, lhs: Self, span: SourceSpan) -> Result<Type, CompileError> {
        match (self, lhs) {
            (Type::I64, Type::I64) => Ok(Type::Bool),
            (Type::Pointer, Type::Pointer) => Ok(Type::Bool),
            _ => Err(CompileError::TypeError2 {
                message: "TODO: Type errors".into(),
                span,
            }),
        }
    }

    fn not(self, span: SourceSpan) -> Result<Type, CompileError> {
        match self {
            Type::Bool => Ok(Type::Bool),
            _ => Err(CompileError::TypeError2 {
                message: format!("Cannot apply '!' to type {}", self),
                span,
            }),
        }
    }

    fn cast_into(
        self,
        target: Self,
        span: SourceSpan,
        out: &mut impl Write,
    ) -> Result<Type, CompileError> {
        match (self, target) {
            (Type::I64, Type::Bool) => {
                writeln!(out, "    popq rax")?;
                writeln!(out, "    cmp rax, 0")?;
                writeln!(out, "    setne al")?;
                writeln!(out, "    movzx rax, al")?;
                writeln!(out, "    pushq rax")?;
                Ok(target)
            }
            _ => Err(CompileError::TypeError2 {
                message: "TODO: Type errors".into(),
                span,
            }),
        }
    }

    fn from_ident(ident: &str) -> Option<Type> {
        match ident {
            "i64" => Some(Self::I64),
            "ptr" => Some(Self::Pointer),
            "fatptr" => Some(Self::FatPointer), // TODO: "fatptr" is bad
            "bool" => Some(Self::Bool),
            _ => None,
        }
    }

    pub fn size(self) -> u32 {
        match self {
            Type::I64 => 8,
            Type::Pointer => 8,
            Type::FatPointer => 8 + 8,
            Type::Bool => 1,
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
pub enum CompileError {
    #[error(transparent)]
    LexError(#[from] LexError),
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error("Stack underflow")]
    StackUnderflow {
        #[label = "because of this"]
        span: SourceSpan,
    },
    #[error("Type Error")]
    TypeError2 {
        #[label = "{message}"]
        span: SourceSpan,
        message: String,
    },
    #[error("Stack not empty at end of program execution.  Contents: {stack:?}")]
    StackNotEmpty { stack: Vec<Type> },
    #[error("'{ident}' defined multiple times")]
    RepeatDefinition {
        ident: String,
        #[label = "original definition here"]
        original: SourceSpan,
        #[label = "repeat definition here"]
        repeat: SourceSpan,
    },
    #[error("Explicit arg size is incorrect.  Expected: {expected}  Found: {actual}")]
    #[diagnostic(help("You may omit arg size for functions which are not variadic."))]
    IncorrectExplicitArgSize {
        expected: usize,
        actual: usize,
        #[label = "here"]
        span: SourceSpan,
    },

    #[error("Variadic arg size is incorrect.  Expected: {expected}  Found: {actual}")]
    #[diagnostic(help(
        "When specifying no variadic args, you may omit the arg size for the function."
    ))]
    IncorrectExplicitVariadicArgSize {
        expected: usize,
        actual: usize,
        #[label = "here"]
        span: SourceSpan,
    },
    #[error("Undefined symbol '{ident}'")]
    UndefinedSymbol {
        ident: String,
        #[label = "here"]
        span: SourceSpan,
    },
    #[error("Stack changed within block.  Before: {before:?}  After: {after:?}")]
    StackChanged {
        before: Vec<Type>,
        after: Vec<Type>,
        #[label = "in this loop"]
        span: SourceSpan,
    },
}

pub struct Compiler<'a, W> {
    cli: &'a Cli,
    ast: &'a [Ast],
    type_stack: Vec<Type>,
    data: HashMap<Box<[u8]>, usize>,
    extern_functions: HashMap<String, ExternFn>,
    out: W,
}

impl<'a, W> Compiler<'a, W>
where
    W: Write,
{
    pub fn new(cli: &'a Cli, ast: &'a [Ast], out: W) -> Self {
        Self {
            cli,
            ast,
            type_stack: Default::default(),
            data: Default::default(),
            extern_functions: Default::default(),
            out,
        }
    }

    fn pop(&mut self, span: SourceSpan) -> Result<Type, CompileError> {
        self.type_stack
            .pop()
            .ok_or(CompileError::StackUnderflow { span })
    }

    fn pop_type(&mut self, span: SourceSpan, expected: Type) -> Result<(), CompileError> {
        let ty = self.pop(span)?;
        if ty != expected {
            return Err(CompileError::TypeError2 {
                span,
                message: format!(
                    "Type error: expected '{}' on stack, found '{}'",
                    expected, ty
                ),
            });
        }
        Ok(())
    }

    pub fn write_header(&mut self) -> Result<(), CompileError> {
        writeln!(self.out, r#"format ELF64"#)?;
        writeln!(self.out, r#"section ".text" executable"#)?;

        for x in self.ast {
            match x {
                Ast::Ident(_) => {}
                Ast::Atom(_) => {}
                Ast::ExternFn(f) => {
                    self.extern_functions.insert(f.name.clone(), f.clone());
                    writeln!(self.out, "extrn {}", f.linker_name)?;
                }
                Ast::Fn(_) => {}
                Ast::Then(_) => {}
                Ast::While(_) => {}
            }
        }

        Ok(())
    }

    fn compile_ident(&mut self, Ident { ident, len, span }: &Ident) -> Result<(), CompileError> {
        if ident == "dump_stack" {
            dbg!(&self.type_stack);
            return Ok(());
        }

        if let Some(ty) = Type::from_ident(ident) {
            let x = self.pop(*span)?;
            self.type_stack.push(x.cast_into(ty, *span, &mut self.out)?);
            return Ok(());
        };

        let Some(ext) = self.extern_functions.get(ident) else {
            return Err(CompileError::UndefinedSymbol {
                ident: ident.to_string(),
                span: *span,
            });
        };
        // TODO
        let ext = ext.clone();

        let len = len.unwrap_or(ext.args.len() as u32);
        let registers = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

        if (ext.variadic && (len as usize) < ext.args.len())
            || (!ext.variadic && ext.args.len() != len as usize)
        {
            if ext.variadic {
                return Err(CompileError::IncorrectExplicitVariadicArgSize {
                    expected: ext.args.len(),
                    actual: len as usize,
                    span: *span,
                });
            } else {
                return Err(CompileError::IncorrectExplicitArgSize {
                    expected: ext.args.len(),
                    actual: len as usize,
                    span: *span,
                });
            }
        }

        for i in 0..len as usize {
            if let Some(r) = registers.get(i) {
                writeln!(self.out, "    popq {}", r)?;
            }

            if i < ext.args.len() {
                self.pop_type(*span, Type::from_ident(&ext.args[i]).expect("TODO"))?;
            } else {
                self.pop(*span)?;
            }
        }

        writeln!(self.out, "    mov rax, 0")?;
        writeln!(self.out, "    call {}", ext.linker_name)?;
        for i in 0..len as usize {
            // Pop _our_ arguments from the stack (this is kinda sus, ngl)
            // TODO: validate this is correct
            if i >= registers.len() {
                writeln!(self.out, "    popq rdi")?;
            }
        }
        if !ext.returns.is_empty() {
            writeln!(self.out, "    pushq rax")?;
        }

        for t in ext.returns {
            self.type_stack.push(Type::from_ident(&t).expect("TODO"));
        }
        Ok(())
    }

    fn compile_atom(&mut self, Atom { kind, token }: &Atom) -> Result<(), CompileError> {
        match kind {
            AtomKind::IntLit(n) => {
                writeln!(self.out, "    pushq {}", n)?;
                self.type_stack.push(Type::I64);
            }
            // TODO: Fat pointers
            AtomKind::StrLit(s) => {
                let n = if let Some(n) = self.data.get(s.as_bytes()) {
                    *n
                } else {
                    let next = self.data.len();
                    let s = s.to_string().into_bytes();
                    self.data.insert(s.into_boxed_slice(), next);
                    next
                };
                self.type_stack.push(Type::FatPointer);
                writeln!(self.out, "    pushq string_{}", n)?;
            }
            AtomKind::CStrLit(s) => {
                let n = if let Some(n) = self.data.get(s.to_bytes_with_nul()) {
                    *n
                } else {
                    let next = self.data.len();
                    let s = s.clone().into_bytes_with_nul();
                    self.data.insert(s.into_boxed_slice(), next);
                    next
                };
                self.type_stack.push(Type::Pointer);
                writeln!(self.out, "    pushq string_{}", n)?;
            }
            AtomKind::Plus => {
                let x = self.pop(token.span)?;
                writeln!(self.out, "    popq rax")?;
                let y = self.pop(token.span)?;
                writeln!(self.out, "    popq rbx")?;
                writeln!(self.out, "    add rbx, rax")?;
                self.type_stack.push(x.add(y, token.span)?);
                writeln!(self.out, "    push rbx")?;
            }
            AtomKind::Minus => {
                let x = self.pop(token.span)?;
                writeln!(self.out, "    popq rax")?;
                let y = self.pop(token.span)?;
                writeln!(self.out, "    popq rbx")?;
                writeln!(self.out, "    sub rbx, rax")?;
                self.type_stack.push(x.sub(y, token.span)?);
                writeln!(self.out, "    push rbx")?;
            }
            AtomKind::Asterisk => todo!("asterisk"),
            AtomKind::Slash => todo!("slash"),
            AtomKind::Equal => {
                let x = self.pop(token.span)?;
                let y = self.pop(token.span)?;
                self.type_stack.push(x.equals(y, token.span)?);
                writeln!(self.out, "    popq rax")?;
                writeln!(self.out, "    popq rbx")?;
                writeln!(self.out, "    cmp rax, rbx")?;
                writeln!(self.out, "    sete al")?;
                writeln!(self.out, "    movzx rax, al")?;
                writeln!(self.out, "    push rax")?;
            }
            AtomKind::Not => {
                let x = self.pop(token.span)?;
                self.type_stack.push(x.not(token.span)?);
                writeln!(self.out, "    popq rax")?;
                writeln!(self.out, "    cmp rax, 0")?;
                writeln!(self.out, "    sete al")?;
                writeln!(self.out, "    movzx rax, al")?;
                writeln!(self.out, "    push rax")?;
            }
            AtomKind::Dup => {
                writeln!(self.out, "    popq rax")?;
                writeln!(self.out, "    pushq rax")?;
                writeln!(self.out, "    pushq rax")?;
                let x = self.pop(token.span)?;
                self.type_stack.push(x);
                self.type_stack.push(x);
            }
            AtomKind::Drop => {
                writeln!(self.out, "    popq rax")?;
                let _ = self.pop(token.span)?;
            }
        }
        Ok(())
    }

    fn compile_while(
        &mut self,
        While {
            while_token,
            condition,
            body,
        }: &While,
    ) -> Result<(), CompileError> {
        let start = self.type_stack.clone();

        // general structure:
        // ```asm
        //     jmp .while_end_0
        // .while_0:
        //     ; body
        //  .while_end_0:
        //     ; condition
        //     popq rax
        //     cmp rax, 0
        //     jne .while_0
        // ```

        writeln!(self.out, "    jmp .while_end_{}", while_token.span.offset())?;
        writeln!(self.out, ".while_{}:", while_token.span.offset())?;
        self.compile_body(body)?;
        if start != self.type_stack {
            // TODO: show a diff instead of before/after?
            return Err(CompileError::StackChanged {
                span: while_token.span,
                before: start,
                after: self.type_stack.clone(),
            });
        }
        writeln!(self.out, ".while_end_{}:", while_token.span.offset())?;
        self.compile_body(condition)?;
        self.pop_type(while_token.span, Type::Bool)?;
        writeln!(self.out, "    popq rax")?;
        writeln!(self.out, "    cmp rax, 0")?;
        writeln!(self.out, "    jne .while_{}", while_token.span.offset())?;

        if start != self.type_stack {
            // TODO: show a diff instead of before/after?
            return Err(CompileError::StackChanged {
                span: while_token.span,
                before: start,
                after: self.type_stack.clone(),
            });
        }
        Ok(())
    }

    fn compile_body(&mut self, ast: &[Ast]) -> Result<(), CompileError> {
        for a in ast {
            match a {
                Ast::Ident(ident) => self.compile_ident(ident)?,
                Ast::Atom(atom) => self.compile_atom(atom)?,
                Ast::ExternFn(_) => {}
                Ast::Fn(_) => todo!(),
                Ast::Then(_) => todo!(),
                Ast::While(w) => self.compile_while(w)?,
            }
            writeln!(self.out)?;
        }

        Ok(())
    }

    pub fn compile(mut self) -> Result<(), CompileError> {
        self.write_header()?;
        writeln!(self.out)?;
        writeln!(self.out, r#"public main"#)?;
        writeln!(self.out, r#"main:"#)?;

        self.compile_body(self.ast)?;

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
                    writeln!(self.out, "    popq rax")?;
                }
            } else {
                return Err(CompileError::StackNotEmpty {
                    stack: self.type_stack,
                });
            }
        }

        writeln!(self.out, "    mov rax, 0")?;
        writeln!(self.out, "    ret")?;

        for (s, n) in self.data {
            writeln!(self.out, "string_{}:", n)?;
            write!(self.out, "    db ")?;
            for (i, c) in s.iter().enumerate() {
                if i > 0 {
                    write!(self.out, ", ")?;
                }
                write!(self.out, "{}", c)?;
            }
            writeln!(self.out)?;
        }

        Ok(())
    }
}
