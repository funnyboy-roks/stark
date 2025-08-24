use std::{borrow::Cow, collections::HashMap, fmt::Display, io::Write, ops::Deref};

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::{
    cli::Cli,
    hash_float::FloatExt,
    lex::{LexError, NumLitVal},
    parse::{Ast, Atom, AtomKind, ExternFn, Fn, Ident, Then, TypeAtom, While},
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

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum FloatRegister {
    Xmm0,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
    Xmm5,
    Xmm6,
    Xmm7,
}

impl FloatRegister {
    pub const ARG_REGS: [Self; 8] = [
        Self::Xmm0,
        Self::Xmm1,
        Self::Xmm2,
        Self::Xmm3,
        Self::Xmm4,
        Self::Xmm5,
        Self::Xmm6,
        Self::Xmm7,
    ];
}

impl Display for FloatRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            FloatRegister::Xmm0 => "xmm0",
            FloatRegister::Xmm1 => "xmm1",
            FloatRegister::Xmm2 => "xmm2",
            FloatRegister::Xmm3 => "xmm3",
            FloatRegister::Xmm4 => "xmm4",
            FloatRegister::Xmm5 => "xmm5",
            FloatRegister::Xmm6 => "xmm6",
            FloatRegister::Xmm7 => "xmm7",
        };
        write!(f, "{}", s)
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    I64,
    I32,
    I16,
    I8,
    U64,
    U32,
    U16,
    U8,
    F32,
    // TODO: typed pointers
    Pointer(Box<Type>),
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
            Type::F32 => write!(f, "f32"),
            Type::Pointer(inner) => write!(f, "ptr<{}>", inner),
            Type::FatPointer => write!(f, "fatptr"),
            Type::Bool => write!(f, "bool"),
        }
    }
}

impl Type {
    fn push(&self, out: &mut impl Write, value: impl Into<Value>) -> Result<(), CompileError> {
        if !self.is_value() {
            todo!();
        }

        // allocate
        ;
        match value.into() {
            // TODO: immediate can have different sizes
            Value::Immediate(n) => {
                writeln!(out, "    mov rax, {}", n)?;
                writeln!(out, "    pushq rax")?;
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

    fn is_float(&self) -> bool {
        match self {
            Type::I64
            | Type::I32
            | Type::I16
            | Type::I8
            | Type::U64
            | Type::U32
            | Type::U16
            | Type::U8
            | Type::Pointer(_)
            | Type::FatPointer
            | Type::Bool => false,
            Type::F32 => true,
        }
    }

    fn pop_float(&self, out: &mut impl Write, register: FloatRegister) -> Result<(), CompileError> {
        if !self.is_value() {
            todo!();
        }

        writeln!(out, "    movss [rsp], {}", register)?;
        writeln!(out, "    add rsp, 4")?;

        Ok(())
    }

    fn pop(&self, out: &mut impl Write, register: Register) -> Result<(), CompileError> {
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
        &self,
        out: &mut impl Write,
        register: Register,
        target: &Self,
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

    fn pop_zeroed(&self, out: &mut impl Write, register: Register) -> Result<(), CompileError> {
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

    fn drop(&self, out: &mut impl Write) -> Result<(), CompileError> {
        if !self.is_value() {
            todo!();
        }

        writeln!(out, "    add rsp, 8")?;

        Ok(())
    }

    const fn is_int(&self) -> bool {
        match self {
            Type::I64
            | Type::I32
            | Type::I16
            | Type::I8
            | Type::U64
            | Type::U32
            | Type::U16
            | Type::U8 => true,
            Type::F32 => false,
            Type::Pointer(_) => false, // TODO: is this an int?
            Type::FatPointer => false,
            Type::Bool => false, // TODO: is this an int?
        }
    }

    const fn is_value(&self) -> bool {
        match self {
            Type::I32 => true,
            Type::I64 => true,
            Type::I16 => true,
            Type::I8 => true,
            Type::U32 => true,
            Type::U64 => true,
            Type::U16 => true,
            Type::U8 => true,
            Type::F32 => true,
            Type::Pointer(_) => true,
            Type::FatPointer => false,
            Type::Bool => true,
        }
    }

    const fn signed(&self) -> bool {
        match self {
            Type::I32 => true,
            Type::I64 => true,
            Type::I16 => true,
            Type::I8 => true,
            Type::U32 => false,
            Type::U64 => false,
            Type::U16 => false,
            Type::U8 => false,
            Type::F32 => true,
            Type::Pointer(_) => false,
            Type::FatPointer => false,
            Type::Bool => false,
        }
    }

    fn add(self, rhs: Self, span: SourceSpan) -> Result<Type, CompileError> {
        match (self, rhs) {
            (a, b) if a.is_int() && a == b => Ok(a),

            (Type::I64, Type::Pointer(t)) => Ok(Type::Pointer(t)),
            (Type::Pointer(t), Type::I64) => Ok(Type::Pointer(t)),
            (Type::Pointer(_), Type::Pointer(_)) => Err(CompileError::TypeError2 {
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
            (a, b) if a.is_int() && a == b => Ok(a),
            (Type::Pointer(a), Type::I64) => Ok(Type::Pointer(a)),
            _ => Err(CompileError::TypeError2 {
                message: "TODO: Type errors".into(),
                span,
            }),
        }
    }

    fn equals(self, lhs: Self, span: SourceSpan) -> Result<Type, CompileError> {
        match (self, lhs) {
            (a, b) if a.is_int() && a == b => Ok(Self::Bool),
            (Type::Pointer(_), Type::Pointer(_)) => Ok(Type::Bool),
            _ => Err(CompileError::TypeError2 {
                message: "TODO: Type errors".into(),
                span,
            }),
        }
    }

    fn modulo(self, lhs: Self, span: SourceSpan) -> Result<Type, CompileError> {
        match (self, lhs) {
            (a, b) if a.is_int() && a == b => Ok(a),
            _ => Err(CompileError::TypeError2 {
                message: "TODO: Type errors".into(),
                span,
            }),
        }
    }

    fn div(self, lhs: Self, span: SourceSpan) -> Result<Type, CompileError> {
        match (self, lhs) {
            (a, b) if a.is_int() && a == b => Ok(a),
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
        &self,
        target: &Self,
        span: SourceSpan,
        out: &mut impl Write,
    ) -> Result<(), CompileError> {
        match (self, target) {
            (Type::I64, Type::Bool) => {
                self.pop(out, Register::Rax)?;
                //writeln!(out, "    popq rax")?;
                writeln!(out, "    cmp rax, 0")?;
                writeln!(out, "    setne al")?;
                writeln!(out, "    movzx rax, al")?;
                target.push(out, Register::Rax)?;
                Ok(())
            }
            // downcast
            (a, b) if a.is_int() && b.is_int() && a.size() > b.size() => {
                writeln!(out, "    ; cast {} into {}", self, target)?;
                self.pop_zeroed(out, Register::Rax)?;
                target.push(out, Register::Rax)?;
                Ok(())
            }
            // upcast
            (a, b) if a.is_int() && b.is_int() && a.size() <= b.size() => {
                writeln!(out, "    ; cast {} into {}", self, target)?;
                self.pop_as(out, Register::Rax, target)?;
                target.push(out, Register::Rax)?;
                Ok(())
            }
            _ => Err(CompileError::TypeError2 {
                message: "TODO: Type errors".into(),
                span,
            }),
        }
    }

    fn from_atom(atom: &TypeAtom) -> Option<Type> {
        match atom {
            TypeAtom::Ident(ident) => {
                match &**ident {
                    "i64" => Some(Self::I64),
                    "i32" => Some(Self::I32),
                    "i16" => Some(Self::I16),
                    "i8" => Some(Self::I8),

                    "u64" => Some(Self::U64),
                    "u32" => Some(Self::U32),
                    "u16" => Some(Self::U16),
                    "u8" => Some(Self::U8),

                    "f32" => Some(Self::F32),

                    "fatptr" => Some(Self::FatPointer), // TODO: "fatptr" is bad
                    "bool" => Some(Self::Bool),
                    _ => None,
                }
            }
            TypeAtom::Pointer(inner) => Some(Self::Pointer(Box::new(Self::from_atom(inner)?))),
        }
    }

    pub const fn size(&self) -> u32 {
        match self {
            Type::I64 | Type::U64 | Type::F32 => 8,
            Type::I32 | Type::U32 => 4,
            Type::I16 | Type::U16 => 2,
            Type::I8 | Type::U8 => 1,
            Type::Pointer(_) => 8,
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

    #[error("Incorrect stack result in function.  Expected: {expected:?}  Actual: {actual:?}")]
    IncorrectStackResults {
        expected: Vec<Type>,
        actual: Vec<Type>,
        #[label = "in this function"]
        span: SourceSpan,
    },
}

#[derive(Default, Debug, Clone)]
pub struct DataItems {
    bytes: HashMap<Box<[u8]>, usize>,
    floats: HashMap<FloatExt<f32>, usize>,
}

impl DataItems {
    pub fn add_bytes(&mut self, bytes: Cow<'_, [u8]>) -> String {
        let n = if let Some(n) = self.bytes.get(bytes.as_ref()) {
            *n
        } else {
            let next = self.bytes.len();
            let s = bytes.into_owned();
            self.bytes.insert(s.into_boxed_slice(), next);
            next
        };
        format!("bytes_{}", n)
    }

    pub fn add_float(&mut self, float: f32) -> String {
        let len = self.floats.len();
        let n = self.floats.entry(float.into()).or_insert(len);
        format!("float_{}", n)
    }

    pub fn write(&self, mut out: impl Write) -> Result<(), CompileError> {
        for (s, n) in &self.bytes {
            writeln!(out, "bytes_{}:", n)?;
            write!(out, "    db ")?;
            for (i, c) in s.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ")?;
                }
                write!(out, "{}", c)?;
            }
            writeln!(out)?;
        }

        for (f, n) in &self.floats {
            writeln!(out, "float_{}:", n)?;
            writeln!(out, "    dd {:?}", **f)?;
        }
        Ok(())
    }
}

pub struct Compiler<'a, W> {
    cli: &'a Cli,
    ast: &'a [Ast],
    type_stack: Vec<Type>,
    data: DataItems,
    extern_functions: HashMap<String, ExternFn>,
    functions: HashMap<String, Fn>,
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
            functions: Default::default(),
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

        self.functions = self
            .ast
            .iter()
            .filter_map(|x| match x {
                Ast::Ident(_) => None,
                Ast::Atom(_) => None,
                Ast::ExternFn(_) => None,
                Ast::Fn(f) => Some((f.name.clone(), f.clone())),
                Ast::Then(_) => None,
                Ast::While(_) => None,
            })
            .collect();

        for x in self.ast {
            match x {
                Ast::Ident(_) => {}
                Ast::Atom(_) => {}
                Ast::ExternFn(f) => {
                    self.extern_functions.insert(f.name.clone(), f.clone());
                    writeln!(self.out, "extrn {}", f.linker_name)?;
                }
                Ast::Fn(f) => {
                    self.functions.insert(f.name.clone(), f.clone());
                    writeln!(self.out, "{}:", f.name)?;

                    // XXX: This is the current implementation of how stark functions are called.
                    // I hate it.  Something better must be created.
                    //
                    // Stack after `call` instruction:
                    //     +--------------- +
                    //     | return pointer |
                    //     +--------------- +
                    //     |     fn arg1    |
                    //     +--------------- +
                    //     |     fn arg0    |
                    //     +----------------+
                    //     |       ...      |
                    //     +----------------+
                    //
                    // Move the return pointer to be _below_ the args
                    //     +--------------- +
                    //     |     fn arg1    |
                    //     +--------------- +
                    //     |     fn arg0    |
                    //     +----------------+
                    //     | return pointer |
                    //     +--------------- +
                    //     |       ...      |
                    //     +----------------+
                    //
                    // After fn body, restore return pointer onto the top stack for `ret`:
                    //     +--------------- +
                    //     | return pointer |
                    //     +--------------- +
                    //     |    return1     |
                    //     +--------------- +
                    //     |    return0     |
                    //     +----------------+
                    //     |       ...      |
                    //     +----------------+

                    writeln!(self.out, "    mov rax, [rsp]")?; // save the return pointer
                    writeln!(self.out)?;
                    writeln!(self.out, "    mov rdi, rsp")?; // rdi is the destination for the `movsq` instruction
                    writeln!(self.out, "    mov rsi, rsp")?; // rsi is the source for the `movsq` instruction
                    writeln!(self.out, "    add rsi, 8")?; // rsi+8 to shift everything down
                    writeln!(self.out)?;
                    writeln!(self.out, "    mov rcx, {}", f.args.len())?; // repeat for each arg
                    writeln!(self.out, "    cld")?; // clear direction flag so `rep` increments
                    writeln!(self.out, "    rep movsq")?;
                    writeln!(self.out, "    mov [rsp+{}], rax", f.args.len() * 8)?; // save the return address on the bottom of the stack

                    let old_stack = std::mem::replace(
                        &mut self.type_stack,
                        f.args
                            .iter()
                            .map(|a| Type::from_atom(a).expect("TODO"))
                            .collect(),
                    );
                    self.compile_body(&f.body)?;
                    let returns = f
                        .returns
                        .iter()
                        .map(|a| Type::from_atom(a).expect("TODO"))
                        .collect();
                    if self.type_stack != returns {
                        return Err(CompileError::IncorrectStackResults {
                            expected: returns,
                            actual: self.type_stack.clone(),
                            span: f.ident.span,
                        });
                    }
                    self.type_stack = old_stack;

                    if !f.returns.is_empty() {
                        writeln!(self.out, "    mov rax, [rsp+{}]", f.returns.len() * 8)?; // get the return address from the bottom of the stack
                        writeln!(self.out)?;
                        writeln!(self.out, "    mov rsi, rsp")?;
                        writeln!(self.out, "    add rsi, {}", (f.returns.len() - 1) * 8)?;
                        writeln!(self.out, "    mov rdi, rsp")?;
                        writeln!(self.out, "    add rdi, {}", f.returns.len() * 8)?;
                        writeln!(self.out)?;
                        writeln!(self.out, "    std")?; // set direction flag so `rep` decrements
                        writeln!(self.out, "    mov rcx, {}", f.returns.len())?;
                        writeln!(self.out, "    rep movsq")?;
                        writeln!(self.out)?;
                        writeln!(self.out, "    mov [rsp], rax")?;
                    }
                    writeln!(self.out, "    ret")?;
                    writeln!(self.out)?;
                }
                Ast::Then(_) => {}
                Ast::While(_) => {}
            }
        }

        Ok(())
    }

    fn call_extern_fn(
        &mut self,
        Ident { len, span, .. }: &Ident,
        ext: ExternFn,
    ) -> Result<(), CompileError> {
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

        let mut inti = 0;
        let mut flti = 0;
        let mut n: usize = 0;
        while n < len as usize {
            if let (Some(r), Some(f)) = (
                Register::ARG_REGS.get(inti),
                FloatRegister::ARG_REGS.get(flti),
            ) {
                let ty = if n < ext.args.len() {
                    self.pop_type(*span, Type::from_atom(&ext.args[n]).expect("TODO"))?
                } else {
                    self.pop(*span)?
                };
                if ty.is_float() {
                    ty.pop_float(&mut self.out, *f)?;
                    flti += 1;
                } else {
                    ty.pop(&mut self.out, *r)?;
                    inti += 1;
                }
                // writeln!(self.out, "    pop {}", r)?;
            } else {
                break;
            }
            n += 1;
        }

        let var_types =
            &self.type_stack[self.type_stack.len() - (len as usize).saturating_sub(n)..];

        writeln!(self.out, "    mov rax, 0")?;
        writeln!(self.out, "    call {}", ext.linker_name)?;
        // Pop _our_ arguments from the stack (this is kinda sus, ngl)
        // TODO: validate this is correct
        for t in var_types {
            t.drop(&mut self.out)?;
            // writeln!(self.out, "    popq rdi")?;
        }
        if !ext.returns.is_empty() {
            let ty = Type::from_atom(&ext.returns[0]).expect("TODO");
            ty.push(&mut self.out, Register::Rax)?;
        }

        for t in ext.returns {
            self.type_stack.push(Type::from_atom(&t).expect("TODO"));
        }
        Ok(())
    }

    fn call_fn(&mut self, Ident { len, span, .. }: &Ident, f: Fn) -> Result<(), CompileError> {
        let len = len.unwrap_or(f.args.len() as u32);
        if f.args.len() != len as usize {
            return Err(CompileError::IncorrectExplicitArgSize {
                expected: f.args.len(),
                actual: len as usize,
                span: *span,
            });
        }

        for i in 0..len as usize {
            // TODO: use linux calling convention
            self.pop_type(*span, Type::from_atom(&f.args[i]).expect("TODO"))?;
            //if let Some(r) = Register::ARG_REGS.get(i) {
            //    let ty = if i < ext.args.len() {
            //    } else {
            //        self.pop(*span)?
            //    };
            //    ty.pop(&mut self.out, *r)?;
            //    // writeln!(self.out, "    pop {}", r)?;
            //}
        }

        // let var_types = &self.type_stack
        //     [self.type_stack.len() - (len as usize).saturating_sub(Register::ARG_REGS.len())..];

        // fn: foo(i32, i32) -> (i32)
        // stack after `call`: [a, b, c, RET]
        // -> [a, RET, d]

        writeln!(self.out, "    mov rax, 0")?;
        writeln!(self.out, "    call {}", f.name)?;

        for t in f.returns {
            self.type_stack.push(Type::from_atom(&t).expect("TODO"));
        }
        Ok(())
    }

    fn compile_ident(&mut self, ident: &Ident) -> Result<(), CompileError> {
        if ident.ident == "dump_stack" {
            dbg!(&self.type_stack);
            return Ok(());
        }

        // TODO: Figure out type casting
        if let Some(ty) = Type::from_atom(&TypeAtom::Ident(ident.ident.clone())) {
            let x = self.pop(ident.span)?;
            x.cast_into(&ty, ident.span, &mut self.out)?;
            self.type_stack.push(ty);
            return Ok(());
        };

        if let Some(ext) = self.extern_functions.get(&ident.ident) {
            self.call_extern_fn(ident, ext.clone())?;
        } else if let Some(f) = self.functions.get(&ident.ident) {
            self.call_fn(ident, f.clone())?;
        } else {
            return Err(CompileError::UndefinedSymbol {
                ident: ident.ident.to_string(),
                span: ident.span,
            });
        };
        // TODO
        Ok(())
    }

    fn compile_atom(&mut self, Atom { kind, token }: &Atom) -> Result<(), CompileError> {
        match kind {
            AtomKind::NumLit(n) => {
                // writeln!(self.out, "    pushq {}", n)?;
                match n.value {
                    NumLitVal::Integer(n) => {
                        Type::I64.push(&mut self.out, n)?;
                        self.type_stack.push(Type::I64);
                    }
                    NumLitVal::Float(f) => {
                        // Type::F32.push(&mut self.out, f)?;
                        let label = self.data.add_float(f);
                        writeln!(self.out, "    movss xmm0, [{}]", label)?;
                        writeln!(self.out, "    sub rsp, 4")?;
                        writeln!(self.out, "    movss [rsp], xmm0")?;
                        self.type_stack.push(Type::F32);
                    }
                }
            }
            // TODO: Fat pointers
            AtomKind::StrLit(s) => {
                let label = self.data.add_bytes(s.as_bytes().into());
                self.type_stack.push(Type::FatPointer);
                // writeln!(self.out, "    pushq string_{}", n)?;
                Type::FatPointer.push(&mut self.out, label)?;
            }
            AtomKind::CStrLit(s) => {
                let label = self.data.add_bytes(s.to_bytes_with_nul().into());
                let ptr = Type::Pointer(Box::new(Type::I8));
                ptr.push(&mut self.out, label)?;
                self.type_stack.push(ptr);
            }
            AtomKind::Type(_) => {
                todo!("This should be an error");
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
                z.push(&mut self.out, Register::Rbx)?;
                self.type_stack.push(z);
                // writeln!(self.out, "    push rbx")?;
            }
            AtomKind::Asterisk => {
                let x = self.pop(token.span)?;
                x.pop(&mut self.out, Register::Rbx)?;
                let y = self.pop(token.span)?;
                y.pop(&mut self.out, Register::Rax)?;
                // TODO: `imul` if sized, else `mul`
                writeln!(self.out, "    imul {}", Register::Rbx.for_size(y.size()))?;
                let z = x.modulo(y, token.span)?;
                z.push(&mut self.out, Register::Rax)?;
                self.type_stack.push(z);
                // writeln!(self.out, "    push rax")?;
            }
            AtomKind::Slash => {
                let x = self.pop(token.span)?;
                x.pop(&mut self.out, Register::Rbx)?;
                let y = self.pop(token.span)?;
                y.pop(&mut self.out, Register::Rax)?;
                // TODO: `idiv` if sized, else `div`
                writeln!(self.out, "    cdq")?;
                writeln!(self.out, "    idiv {}", Register::Rbx.for_size(y.size()))?;
                let z = x.div(y, token.span)?;
                z.push(&mut self.out, Register::Rax)?;
                self.type_stack.push(z);
                // writeln!(self.out, "    push rax")?;
            }
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
                z.push(&mut self.out, Register::Rax)?;
                self.type_stack.push(z);
            }
            AtomKind::Not => {
                let x = self.pop(token.span)?;
                x.pop(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    popq rax")?;
                writeln!(self.out, "    cmp {}, 0", Register::Rax.for_size(x.size()))?;
                writeln!(self.out, "    sete al")?;
                writeln!(self.out, "    movzx rax, al")?;
                let y = x.not(token.span)?;
                y.push(&mut self.out, Register::Rax)?;
                self.type_stack.push(y);
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
            AtomKind::Percent => {
                let x = self.pop(token.span)?;
                x.pop(&mut self.out, Register::Rbx)?;
                let y = self.pop(token.span)?;
                y.pop(&mut self.out, Register::Rax)?;
                // TODO: `idiv` if sized, else `div`
                writeln!(self.out, "    cdq")?;
                writeln!(self.out, "    idiv {}", Register::Rbx.for_size(y.size()),)?;
                let z = x.modulo(y, token.span)?;
                z.push(&mut self.out, Register::Rdx)?;
                self.type_stack.push(z);
            }
            AtomKind::Dup => {
                let x = self.pop(token.span)?;
                let size = x.size();
                assert!(size % 8 == 0);
                writeln!(self.out, "    mov rsi, rsp")?;
                writeln!(self.out, "    sub rsp, {}", size)?;
                writeln!(self.out, "    mov rdi, rsp")?;
                writeln!(self.out, "    mov rcx, {}", size / 8)?;
                writeln!(self.out, "    rep movsq")?;
                self.type_stack.push(x.clone());
                self.type_stack.push(x);
            }
            AtomKind::Dup2 => {
                let x = self.pop(token.span)?;
                self.type_stack.push(x.clone());
                let y = self.pop(token.span)?;
                self.type_stack.push(y.clone());

                let size = x.size() + y.size();
                assert!(size % 8 == 0);
                writeln!(self.out, "    mov rsi, rsp")?;
                writeln!(self.out, "    sub rsp, {}", size)?;
                writeln!(self.out, "    mov rdi, rsp")?;
                writeln!(self.out, "    mov rcx, {}", size / 8)?;
                writeln!(self.out, "    rep movsq")?;

                self.type_stack.push(y);
                self.type_stack.push(x);
            }
            AtomKind::Swap => {
                let x = self.pop(token.span)?;
                x.pop(&mut self.out, Register::Rax)?;
                // writeln!(self.out, "    popq rax")?;
                let y = self.pop(token.span)?;
                y.pop(&mut self.out, Register::Rbx)?;
                // writeln!(self.out, "    popq rbx")?;

                x.push(&mut self.out, Register::Rax)?;
                self.type_stack.push(x);
                // writeln!(self.out, "    pushq rax")?;
                y.push(&mut self.out, Register::Rbx)?;
                self.type_stack.push(y);
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
        if elze.is_some() {
            writeln!(self.out, "    je .else_{}_{}", id, 0)?;
        } else {
            writeln!(self.out, "    je .then_end_{}", id)?;
        }
        self.compile_body(body)?;
        writeln!(self.out, "    jmp .then_end_{}", id)?;
        for (i, et) in else_thens.iter().enumerate() {
            let then_end = std::mem::replace(&mut self.type_stack, start.clone());
            writeln!(self.out, ".else_{}_{}:", id, i)?;
            self.compile_body(&et.condition)?;
            self.pop_type(then_token.span, Type::Bool)?;
            writeln!(self.out, "    popq rax")?;
            writeln!(self.out, "    cmp rax, 0")?;
            if i == else_thens.len() - 1 && elze.is_none() {
                writeln!(self.out, "    je .then_end_{}", id)?;
            } else {
                writeln!(self.out, "    je .else_{}_{}", id, i + 1)?;
            }
            self.compile_body(&et.body)?;
            writeln!(self.out, "    jmp .then_end_{}", id)?;
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
            writeln!(self.out, ".else_{}_{}:", id, else_thens.len())?;
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
        writeln!(self.out, ".then_end_{}:", id)?;
        Ok(())
    }

    fn compile_body(&mut self, ast: &[Ast]) -> Result<(), CompileError> {
        for a in ast {
            match a {
                Ast::Ident(ident) => self.compile_ident(ident)?,
                Ast::Atom(atom) => self.compile_atom(atom)?,
                Ast::ExternFn(_) => continue,
                Ast::Fn(_) => continue,
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
        writeln!(self.out, r#"    push rbp"#)?;
        writeln!(self.out, r#"    mov rbp, rsp"#)?;

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
        writeln!(self.out, "    pop rbp")?;
        writeln!(self.out, "    ret")?;
        writeln!(self.out)?;

        self.data.write(&mut self.out)?;

        Ok(())
    }
}
