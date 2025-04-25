use std::{collections::HashMap, fmt::Display, io::Write};

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::{
    cli::Cli,
    lex::LexError,
    parse::{Ast, Atom, AtomKind, ExternFn, Ident, Then, While},
};

/// Docs from https://wiki.osdev.org/CPU_Registers_x86-64#General_Purpose_Registers
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Register {
    /// Accumulator
    Rax,
    /// Base
    Rbx,
    /// Counter
    Rcx,
    /// Data (commonly extends the A register)
    Rdx,
    /// Source index for string operations
    Rsi,
    /// Destination index for string operations
    Rdi,
    /// Stack Pointer
    Rsp,
    /// Base Pointer (meant for stack frames)
    Rbp,
    /// General purpose
    R8,
    /// General purpose
    R9,
    /// General purpose
    R10,
    /// General purpose
    R11,
    /// General purpose
    R12,
    /// General purpose
    R13,
    /// General purpose
    R14,
    /// General purpose
    R15,
}

impl Register {
    pub const ARG_REGS: [Self; 6] = [
        Self::Rdi,
        Self::Rsi,
        Self::Rdx,
        Self::Rcx,
        Self::R8,
        Self::R9,
    ];

    pub fn for_size(self, size: u32) -> &'static str {
        macro_rules! registers {
            (N = $c: literal) => {
                match size {
                    1 => concat!("r", $c, "b"),
                    2 => concat!("r", $c, "w"),
                    4 => concat!("r", $c, "d"),
                    8 => concat!("r", $c),
                    _ => panic!("invalid size: {}", size),
                }
            };
            ($c: literal) => {
                match size {
                    1 => concat!($c, "l"),
                    2 => concat!($c, "x"),
                    4 => concat!("e", $c, "x"),
                    8 => concat!("r", $c, "x"),
                    _ => panic!("invalid size: {}", size),
                }
            };
            ($qword: literal, $dword: literal, $word: literal, $byte: literal) => {
                match size {
                    1 => $byte,
                    2 => $word,
                    4 => $dword,
                    8 => $qword,
                    _ => panic!("invalid size: {}", size),
                }
            };
        }

        match self {
            Register::Rax => registers!("a"),
            Register::Rbx => registers!("b"),
            Register::Rcx => registers!("c"),
            Register::Rdx => registers!("d"),
            Register::Rsi => registers!("rsi", "esi", "si", "sil"),
            Register::Rdi => registers!("rdi", "edi", "di", "dil"),
            Register::Rsp => registers!("rsp", "esp", "sp", "spl"),
            Register::Rbp => registers!("rbp", "ebp", "bp", "bpl"),
            Register::R8 => registers!(N = 8),
            Register::R9 => registers!(N = 9),
            Register::R10 => registers!(N = 10),
            Register::R11 => registers!(N = 11),
            Register::R12 => registers!(N = 12),
            Register::R13 => registers!(N = 13),
            Register::R14 => registers!(N = 14),
            Register::R15 => registers!(N = 15),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Value {
    Immediate(i64),
    Label(String),
    Register(Register),
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::Label(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Immediate(value)
    }
}

impl From<Register> for Value {
    fn from(value: Register) -> Self {
        Self::Register(value)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Type {
    I64,
    I32,
    I16,
    I8,
    U64,
    U32,
    U16,
    U8,
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
            Type::I32 => write!(f, "i32"),
            Type::I16 => write!(f, "i16"),
            Type::I8 => write!(f, "i8"),
            Type::U64 => write!(f, "u64"),
            Type::U32 => write!(f, "u32"),
            Type::U16 => write!(f, "u16"),
            Type::U8 => write!(f, "u8"),
            Type::Pointer => write!(f, "ptr"),
            Type::FatPointer => write!(f, "fatptr"),
            Type::Bool => write!(f, "bool"),
        }
    }
}

impl Type {
    fn push(self, out: &mut impl Write, value: impl Into<Value>) -> Result<(), CompileError> {
        if !self.is_value() {
            todo!();
        }

        // allocate
        ;
        match value.into() {
            // TODO: immediate can have different sizes
            Value::Immediate(n) => {
                writeln!(out, "    pushq {}", n)?;
            }
            Value::Label(l) => writeln!(out, "    push {}", l)?,
            Value::Register(register) => {
                writeln!(out, "    pushq {}", register.for_size(8))?;
                // writeln!(out, "    sub rsp, 8")?;
                // writeln!(
                //     out,
                //     "    mov {} [rsp+{}] {}",
                //     pneumonic(self.size()),
                //     8 - self.size(),
                //     register.for_size(self.size())
                // )?;
            }
        };

        Ok(())
    }

    fn pop(self, out: &mut impl Write, register: Register) -> Result<(), CompileError> {
        if !self.is_value() {
            todo!();
        }

        writeln!(out, "    popq {}", register.for_size(8))?;
        if self.size() < 4 {
            writeln!(
                out,
                "    {} {}, {}",
                if self.signed() { "movsx" } else { "movzx" },
                register.for_size(8),
                register.for_size(self.size())
            )?;
        }

        Ok(())
    }

    fn pop_as(
        self,
        out: &mut impl Write,
        register: Register,
        target: Self,
    ) -> Result<(), CompileError> {
        if !self.is_value() {
            todo!();
        }

        writeln!(out, "    popq {}", register.for_size(8))?;
        if self.size() < 4 {
            writeln!(
                out,
                "    {} {}, {}",
                if target.signed() { "movsx" } else { "movzx" },
                register.for_size(8),
                register.for_size(self.size())
            )?;
        }

        Ok(())
    }

    fn pop_zeroed(self, out: &mut impl Write, register: Register) -> Result<(), CompileError> {
        if !self.is_value() {
            todo!();
        }

        writeln!(out, "    popq {}", register.for_size(8))?;
        if self.size() < 4 {
            writeln!(
                out,
                "    movzx {}, {}",
                register.for_size(8),
                register.for_size(self.size())
            )?;
        }

        Ok(())
    }

    fn drop(self, out: &mut impl Write) -> Result<(), CompileError> {
        if !self.is_value() {
            todo!();
        }

        writeln!(out, "    add rsp, 8")?;

        Ok(())
    }

    const fn is_int(self) -> bool {
        match self {
            Type::I64
            | Type::I32
            | Type::I16
            | Type::I8
            | Type::U64
            | Type::U32
            | Type::U16
            | Type::U8 => true,
            Type::Pointer => false, // TODO: is this an int?
            Type::FatPointer => false,
            Type::Bool => false, // TODO: is this an int?
        }
    }

    const fn is_value(self) -> bool {
        match self {
            Type::I32 => true,
            Type::I64 => true,
            Type::I16 => true,
            Type::I8 => true,
            Type::U32 => true,
            Type::U64 => true,
            Type::U16 => true,
            Type::U8 => true,
            Type::Pointer => true,
            Type::FatPointer => false,
            Type::Bool => true,
        }
    }

    const fn signed(self) -> bool {
        match self {
            Type::I32 => true,
            Type::I64 => true,
            Type::I16 => true,
            Type::I8 => true,
            Type::U32 => false,
            Type::U64 => false,
            Type::U16 => false,
            Type::U8 => false,
            Type::Pointer => false,
            Type::FatPointer => false,
            Type::Bool => false,
        }
    }

    fn add(self, rhs: Self, span: SourceSpan) -> Result<Type, CompileError> {
        match (self, rhs) {
            (a, b) if a.is_int() && a == b => Ok(a),

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
                self.pop(out, Register::Rax)?;
                //writeln!(out, "    popq rax")?;
                writeln!(out, "    cmp rax, 0")?;
                writeln!(out, "    setne al")?;
                writeln!(out, "    movzx rax, al")?;
                target.push(out, Register::Rax)?;
                Ok(target)
            }
            // downcast
            (a, b) if a.is_int() && b.is_int() && a.size() > b.size() => {
                writeln!(out, "    ; cast {} into {}", self, target)?;
                self.pop_zeroed(out, Register::Rax)?;
                target.push(out, Register::Rax)?;
                Ok(target)
            }
            // upcast
            (a, b) if a.is_int() && b.is_int() && a.size() <= b.size() => {
                writeln!(out, "    ; cast {} into {}", self, target)?;
                self.pop_as(out, Register::Rax, target)?;
                target.push(out, Register::Rax)?;
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
            "i32" => Some(Self::I32),
            "i16" => Some(Self::I16),
            "i8" => Some(Self::I8),

            "u64" => Some(Self::U64),
            "u32" => Some(Self::U32),
            "u16" => Some(Self::U16),
            "u8" => Some(Self::U8),

            "ptr" => Some(Self::Pointer),
            "fatptr" => Some(Self::FatPointer), // TODO: "fatptr" is bad
            "bool" => Some(Self::Bool),
            _ => None,
        }
    }

    pub const fn size(self) -> u32 {
        match self {
            Type::I64 | Type::U64 => 8,
            Type::I32 | Type::U32 => 4,
            Type::I16 | Type::U16 => 2,
            Type::I8 | Type::U8 => 1,
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

    fn pop_type(&mut self, span: SourceSpan, expected: Type) -> Result<Type, CompileError> {
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
        Ok(expected)
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
            if let Some(r) = Register::ARG_REGS.get(i) {
                let ty = if i < ext.args.len() {
                    self.pop_type(*span, Type::from_ident(&ext.args[i]).expect("TODO"))?
                } else {
                    self.pop(*span)?
                };
                ty.pop(&mut self.out, *r)?;
                // writeln!(self.out, "    pop {}", r)?;
            }
        }

        let var_types = &self.type_stack
            [self.type_stack.len() - (len as usize).saturating_sub(Register::ARG_REGS.len())..];

        writeln!(self.out, "    mov rax, 0")?;
        writeln!(self.out, "    call {}", ext.linker_name)?;
        // Pop _our_ arguments from the stack (this is kinda sus, ngl)
        // TODO: validate this is correct
        for t in var_types {
            t.drop(&mut self.out)?;
            // writeln!(self.out, "    popq rdi")?;
        }
        if !ext.returns.is_empty() {
            let ty = Type::from_ident(&ext.returns[0]).expect("TODO");
            ty.push(&mut self.out, Register::Rax)?;
        }

        for t in ext.returns {
            self.type_stack.push(Type::from_ident(&t).expect("TODO"));
        }
        Ok(())
    }

    fn compile_atom(&mut self, Atom { kind, token }: &Atom) -> Result<(), CompileError> {
        match kind {
            AtomKind::IntLit(n) => {
                // writeln!(self.out, "    pushq {}", n)?;
                Type::I64.push(&mut self.out, *n)?;
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
                // writeln!(self.out, "    pushq string_{}", n)?;
                Type::Pointer.push(&mut self.out, format!("string_{}", n))?;
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
                // writeln!(self.out, "    pushq string_{}", n)?;
                Type::Pointer.push(&mut self.out, format!("string_{}", n))?;
            }
            AtomKind::Plus => {
                let x = self.pop(token.span)?;
                x.pop(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    popq rax")?;
                let y = self.pop(token.span)?;
                // writeln!(self.out, "    popq rbx")?;
                y.pop(&mut self.out, Register::Rbx)?;
                writeln!(
                    self.out,
                    "    add {}, {}",
                    Register::Rbx.for_size(y.size()),
                    Register::Rax.for_size(x.size())
                )?;
                let z = x.add(y, token.span)?;
                self.type_stack.push(z);
                writeln!(self.out, "    push rbx")?;
            }
            AtomKind::Minus => {
                let x = self.pop(token.span)?;
                x.pop(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    popq rax")?;
                let y = self.pop(token.span)?;
                // writeln!(self.out, "    popq rbx")?;
                y.pop(&mut self.out, Register::Rbx)?;
                writeln!(
                    self.out,
                    "    sub {}, {}",
                    Register::Rbx.for_size(y.size()),
                    Register::Rax.for_size(x.size())
                )?;
                let z = x.sub(y, token.span)?;
                self.type_stack.push(z);
                z.push(&mut self.out, Register::Rbx)?;
                // writeln!(self.out, "    push rbx")?;
            }
            AtomKind::Asterisk => todo!("asterisk"),
            AtomKind::Slash => todo!("slash"),
            AtomKind::Equal => {
                // let z = x == y;
                let x = self.pop(token.span)?;
                x.pop(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    popq rax")?;
                let y = self.pop(token.span)?;
                y.pop(&mut self.out, Register::Rbx)?;
                writeln!(
                    self.out,
                    "    cmp {}, {}",
                    Register::Rbx.for_size(y.size()),
                    Register::Rax.for_size(x.size())
                )?;
                writeln!(self.out, "    sete al")?;
                writeln!(self.out, "    movzx rax, al")?;
                let z = x.equals(y, token.span)?;
                self.type_stack.push(z);
                z.push(&mut self.out, Register::Rax)?;
            }
            AtomKind::Not => {
                let x = self.pop(token.span)?;
                x.pop(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    popq rax")?;
                writeln!(self.out, "    cmp {}, 0", Register::Rax.for_size(x.size()))?;
                writeln!(self.out, "    sete al")?;
                writeln!(self.out, "    movzx rax, al")?;
                let y = x.not(token.span)?;
                self.type_stack.push(y);
                y.push(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    push rax")?;
            }
            AtomKind::Lt => {
                let x = self.pop(token.span)?;
                x.pop(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    popq rax")?;
                let y = self.pop(token.span)?;
                y.pop(&mut self.out, Register::Rbx)?;
                // writeln!(self.out, "    popq rbx")?;
                writeln!(
                    self.out,
                    "    cmp {}, {}",
                    Register::Rbx.for_size(y.size()),
                    Register::Rax.for_size(x.size())
                )?;
                writeln!(self.out, "    setl al")?;
                writeln!(self.out, "    movzx rax, al")?;
                let z = x.equals(y, token.span)?;
                z.push(&mut self.out, Register::Rax)?;
                self.type_stack.push(z);
                // writeln!(self.out, "    push rax")?;
            }
            AtomKind::Dup => {
                let x = self.pop(token.span)?;
                x.pop(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    popq rax")?;
                self.type_stack.push(x);
                x.push(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    pushq rax")?;
                self.type_stack.push(x);
                x.push(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    pushq rax")?;
            }
            AtomKind::Dup2 => {
                let x = self.pop(token.span)?;
                x.pop(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    popq rax")?;
                let y = self.pop(token.span)?;
                y.pop(&mut self.out, Register::Rbx)?;
                // writeln!(self.out, "    popq rbx")?;

                self.type_stack.push(y);
                y.push(&mut self.out, Register::Rbx)?;
                // writeln!(self.out, "    pushq rbx")?;
                self.type_stack.push(x);
                x.push(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    pushq rax")?;
                self.type_stack.push(y);
                y.push(&mut self.out, Register::Rbx)?;
                // writeln!(self.out, "    pushq rbx")?;
                self.type_stack.push(x);
                x.push(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    pushq rax")?;
            }
            AtomKind::Swap => {
                let x = self.pop(token.span)?;
                x.pop(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    popq rax")?;
                let y = self.pop(token.span)?;
                y.pop(&mut self.out, Register::Rbx)?;
                // writeln!(self.out, "    popq rbx")?;

                self.type_stack.push(x);
                x.push(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    pushq rax")?;
                self.type_stack.push(y);
                y.push(&mut self.out, Register::Rbx)?;
                // writeln!(self.out, "    pushq rbx")?;
            }
            AtomKind::Drop => {
                let x = self.pop(token.span)?;
                x.drop(&mut self.out)?;
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

    fn compile_then(
        &mut self,
        Then {
            then_token,
            body,
            else_thens,
            elze,
        }: &Then,
    ) -> Result<(), CompileError> {
        let id = then_token.span.offset();
        self.pop_type(then_token.span, Type::Bool)?;
        let start = self.type_stack.clone();
        writeln!(self.out, "    popq rax")?;
        writeln!(self.out, "    cmp rax, 0")?;
        writeln!(self.out, "    jne .then_end_{}", id)?;
        self.compile_body(body)?;
        writeln!(self.out, ".then_end_{}:", id)?;
        for (i, et) in else_thens.iter().enumerate() {
            let then_end = std::mem::replace(&mut self.type_stack, start.clone());
            self.compile_body(&et.condition)?;
            self.pop_type(then_token.span, Type::Bool)?;
            writeln!(self.out, "    popq rax")?;
            writeln!(self.out, "    cmp rax, 0")?;
            writeln!(self.out, "    jne .then_end_{}_{}", id, i)?;
            self.compile_body(&et.body)?;
            writeln!(self.out, ".then_end_{}_{}:", id, i)?;
            if then_end != self.type_stack {
                // TODO: show a diff instead of before/after?
                return Err(CompileError::StackChanged {
                    span: et.then_token.span,
                    before: then_end,
                    after: self.type_stack.clone(),
                });
            }
        }
        if let Some((else_body, else_token)) = elze {
            let then_end = std::mem::replace(&mut self.type_stack, start.clone());
            self.compile_body(else_body)?;
            if then_end != self.type_stack {
                // TODO: show a diff instead of before/after?
                return Err(CompileError::StackChanged {
                    span: else_token.span,
                    before: then_end,
                    after: self.type_stack.clone(),
                });
            }
        } else if start != self.type_stack {
            // TODO: show a diff instead of before/after?
            return Err(CompileError::StackChanged {
                span: then_token.span,
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
                Ast::ExternFn(_) => continue,
                Ast::Fn(_) => todo!(),
                Ast::Then(t) => self.compile_then(t)?,
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
        writeln!(self.out)?;

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
