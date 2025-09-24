use std::{borrow::Cow, collections::HashMap, fmt::Display, io::Write, ops::DerefMut};

use miette::Diagnostic;
use thiserror::Error;

use crate::{
    hash_float::FloatExt,
    ir::{
        Builtin, Cast, ConvertedFunction, FloatLitType, FunctionSignature, Ir, IrGenError, IrKind,
        Module, ThenIr, Type, TypeStack, WhileIr,
    },
    parse::Ident,
    span::{Span, Spanned},
    ty,
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
        match self {
            FloatRegister::Xmm0 => write!(f, "xmm0"),
            FloatRegister::Xmm1 => write!(f, "xmm1"),
            FloatRegister::Xmm2 => write!(f, "xmm2"),
            FloatRegister::Xmm3 => write!(f, "xmm3"),
            FloatRegister::Xmm4 => write!(f, "xmm4"),
            FloatRegister::Xmm5 => write!(f, "xmm5"),
            FloatRegister::Xmm6 => write!(f, "xmm6"),
            FloatRegister::Xmm7 => write!(f, "xmm7"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ArgumentDest {
    Register,
    Stack,
    FloatRegister,
}

impl Type {
    /// Where this type should go for an argument to a function
    pub fn argument_dest(&self) -> ArgumentDest {
        match self {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Integer => ArgumentDest::Register,
            Type::F32 | Type::F64 | Type::Float => ArgumentDest::FloatRegister,
            Type::Pointer(_) | Type::VoidPointer => ArgumentDest::Register,
            Type::FatPointer => todo!(),
            Type::Struct => todo!(),
            Type::Bool => ArgumentDest::Register,
        }
    }

    fn pop_as(
        &self,
        writer: &mut impl Write,
        register: Register,
        target: &Self,
    ) -> Result<(), CodeGenError> {
        if !self.is_integer() {
            todo!("Type::pop_as for non-integer");
        }

        writeln!(writer, "    popq {}", register.for_size(8))?;
        if self.size() < target.size() {
            writeln!(
                writer,
                "    {} {}, {}",
                if target.is_signed() { "movsx" } else { "movzx" },
                register.for_size(8),
                register.for_size(self.size())
            )?;
        }

        Ok(())
    }
}

#[derive(Default, Debug, Clone)]
pub struct DataItems {
    bytes: HashMap<Box<[u8]>, usize>,
    // (float, double precision)
    floats: HashMap<(FloatExt<f64>, bool), usize>,
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

    pub fn add_f32(&mut self, float: f32) -> String {
        let len = self.floats.len();
        let n = self
            .floats
            .entry(((float as f64).into(), false))
            .or_insert(len);
        format!("float_{}", n)
    }

    pub fn add_f64(&mut self, float: f64) -> String {
        let len = self.floats.len();
        let n = self.floats.entry((float.into(), true)).or_insert(len);
        format!("float_{}", n)
    }

    pub fn write(&self, mut out: impl Write) -> Result<(), CodeGenError> {
        for (s, n) in &self.bytes {
            writeln!(out, "bytes_{}: ; {:?}", n, String::from_utf8_lossy(s))?;
            write!(out, "    db ")?;
            for (i, c) in s.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ")?;
                }
                write!(out, "{}", c)?;
            }
            writeln!(out)?;
        }

        for ((f, double_precision), n) in &self.floats {
            writeln!(out, "float_{}:", n)?;
            if *double_precision {
                writeln!(out, "    dq {:?}", **f)?;
            } else {
                writeln!(out, "    dd {:?}", **f)?;
            }
        }
        Ok(())
    }
}

impl Type {
    pub fn drop<W: Write>(&self, writer: &mut W) -> Result<(), CodeGenError> {
        writeln!(writer, "    add rsp, {}", self.padded_size())?;
        Ok(())
    }
}

impl Builtin {
    pub fn compile_to<W: Write>(
        self,
        writer: &mut W,
        type_stack: &mut TypeStack,
    ) -> Result<(), CodeGenError> {
        let len = type_stack.len();
        macro_rules! compare {
            ($first: ident, $second: ident) => {
                if type_stack[len - 1].is_float() {
                    let top = &type_stack[len - 1];
                    let bottom = &type_stack[len - 2];
                    assert_eq!(top.padded_size(), 8);
                    assert_eq!(bottom.padded_size(), 8);
                    let infer_one = top.is_unresolved() || bottom.is_unresolved();
                    let size = top.size().min(bottom.size());
                    if infer_one || top.size() == bottom.size() {
                        let infer_bottom = bottom.is_unresolved();
                        if size == 4 {
                            if infer_one {
                                if infer_bottom {
                                    writeln!(writer, "    movss {}, [rsp]", FloatRegister::Xmm0)?;
                                    writeln!(writer, "    cvtsd2ss {}, [rsp+8]", FloatRegister::Xmm1)?;
                                } else {
                                    writeln!(writer, "    cvtsd2ss {}, [rsp]", FloatRegister::Xmm0)?;
                                    writeln!(writer, "    movss {}, [rsp+8]", FloatRegister::Xmm1)?;
                                }
                            }
                            writeln!(writer, "    add rsp, 16")?;
                            writeln!(
                                writer,
                                "    comiss {}, {}",
                                compare!(@f $first),
                                compare!(@f $second),
                            )?;
                        } else if size == 8 {
                            if infer_one {
                                if infer_bottom {
                                    writeln!(writer, "    movsd {}, [rsp]", FloatRegister::Xmm0)?;
                                    writeln!(writer, "    movsd {}, [rsp+8]", FloatRegister::Xmm1)?;
                                } else {
                                    writeln!(writer, "    movsd {}, [rsp]", FloatRegister::Xmm0)?;
                                    writeln!(writer, "    movsd {}, [rsp+8]", FloatRegister::Xmm1)?;
                                }
                            }
                            writeln!(writer, "    add rsp, 16")?;
                            writeln!(
                                writer,
                                "    comisd {}, {}",
                                compare!(@f $first),
                                compare!(@f $second),
                            )?;
                        }
                    } else {
                        unreachable!();
                    }
                } else {
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    writeln!(writer, "    popq rax")?;
                    writeln!(writer, "    popq rbx")?;
                    writeln!(writer, "    cmp {}, {}", compare!(@n $first), compare!(@n $second))?;
                }
            };
            (@f a) => { FloatRegister::Xmm0 };
            (@f b) => { FloatRegister::Xmm1 };
            (@n a) => { "rax" };
            (@n b) => { "rbx" };
        }
        match self {
            Builtin::Add => {
                if type_stack[len - 1].is_float() {
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    // equiv to:
                    //     movss xmm0, [rsp]
                    //     movss xmm1, [rsp+8]
                    //     add rsp, 8
                    //     addss xmm1, xmm0
                    //     sub rsp, 8
                    //     movss [rsp], xmm1
                    writeln!(writer, "    movss xmm0, [rsp]")?;
                    writeln!(writer, "    add rsp, 8")?;
                    writeln!(writer, "    addss xmm0, [rsp]")?;
                    writeln!(writer, "    movss [rsp], xmm0")?;
                } else {
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    assert!(type_stack[len - 1].is_integer());
                    // equiv to:
                    //     popq rbx
                    //     popq rax
                    //     add rax, rbx
                    //     push rax
                    let n = if let Type::Pointer(x) = &type_stack[len - 1] {
                        assert_eq!(x.size().count_ones(), 1, "x.size() is a power of two");
                        let n = x.size().trailing_zeros();
                        assert!(2u32.pow(n) == x.size());
                        writeln!(writer, "    popq rbx")?;
                        writeln!(writer, "    popq rax")?;
                        writeln!(writer, "    pushq rbx")?;
                        n
                    } else if let Type::Pointer(x) = &type_stack[len - 2] {
                        assert_eq!(x.size().count_ones(), 1, "x.size() is a power of two");
                        let n = x.size().trailing_zeros();
                        assert!(2u32.pow(n) == x.size());
                        writeln!(writer, "    popq rax")?;
                        n
                    } else {
                        writeln!(writer, "    popq rax")?;
                        0
                    };

                    if n != 0 {
                        writeln!(writer, "    shl rax, {}", n)?;
                    }
                    writeln!(writer, "    add [rsp], rax")?;
                }
            }
            Builtin::Sub => {
                if type_stack[len - 1].is_float() {
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    // equiv to:
                    //     movss xmm0, [rsp]
                    //     movss xmm1, [rsp+8]
                    //     add rsp, 8
                    //     subss xmm1, xmm0
                    //     sub rsp, 8
                    //     movss [rsp], xmm1
                    writeln!(writer, "    movss xmm1, [rsp+8]")?;
                    writeln!(writer, "    subss xmm1, [rsp]")?;
                    writeln!(writer, "    add rsp, 8")?;
                    writeln!(writer, "    movss [rsp], xmm1")?;
                } else {
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    assert!(type_stack[len - 1].is_integer());
                    writeln!(writer, "    popq rax")?;
                    writeln!(writer, "    sub [rsp], rax")?;
                }
            }
            Builtin::Mul => {
                let ty = &type_stack[len - 1];
                if ty.is_float() {
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    // equiv to:
                    //     movss xmm0, [rsp]
                    //     movss xmm1, [rsp+8]
                    //     add rsp, 8
                    //     mulss xmm1, xmm0
                    //     sub rsp, 8
                    //     movss [rsp], xmm1
                    // writeln!(writer, "    movss xmm1, [rsp+8]")?;
                    // writeln!(writer, "    mulss xmm1, [rsp]")?;
                    // writeln!(writer, "    add rsp, 8")?;
                    // writeln!(writer, "    movss [rsp], xmm1")?;
                    writeln!(writer, "    movss xmm0, [rsp]")?;
                    writeln!(writer, "    add rsp, 8")?;
                    writeln!(writer, "    mulss xmm0, [rsp]")?;
                    writeln!(writer, "    movss [rsp], xmm0")?;
                } else {
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    assert!(type_stack[len - 1].is_integer());
                    writeln!(writer, "    popq {}", Register::Rax.for_size(ty.size()))?;
                    writeln!(writer, "    popq {}", Register::Rbx.for_size(ty.size()))?;
                    writeln!(
                        writer,
                        "    {} {}",
                        if ty.is_signed() { "imul" } else { "mul" },
                        Register::Rbx.for_size(ty.size())
                    )?;
                    writeln!(writer, "    push {}", Register::Rax.for_size(ty.size()))?;
                }
            }
            Builtin::Div => {
                let ty = &type_stack[len - 1];
                if ty.is_float() {
                    todo!("float div");
                } else {
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    assert!(type_stack[len - 1].is_integer());
                    writeln!(
                        writer,
                        "    xor {0}, {0}",
                        Register::Rdx.for_size(ty.size())
                    )?;
                    writeln!(writer, "    popq {}", Register::Rbx.for_size(ty.size()))?;
                    writeln!(writer, "    popq {}", Register::Rax.for_size(ty.size()))?;
                    writeln!(
                        writer,
                        "    {} {}",
                        if ty.is_signed() { "idiv" } else { "div" },
                        Register::Rbx.for_size(ty.size())
                    )?;
                    // rdx is the quotent
                    writeln!(writer, "    push {}", Register::Rax.for_size(ty.size()))?;
                }
            }
            Builtin::Mod => {
                let ty = &type_stack[len - 1];
                if ty.is_float() {
                    todo!("float mod");
                } else {
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    assert!(type_stack[len - 1].is_integer());
                    writeln!(
                        writer,
                        "    xor {0}, {0}",
                        Register::Rdx.for_size(ty.size())
                    )?;
                    writeln!(writer, "    popq {}", Register::Rbx.for_size(ty.size()))?;
                    writeln!(writer, "    popq {}", Register::Rax.for_size(ty.size()))?;
                    writeln!(
                        writer,
                        "    {} {}",
                        if ty.is_signed() { "idiv" } else { "div" },
                        Register::Rbx.for_size(ty.size())
                    )?;
                    // rdx is the remainder
                    writeln!(writer, "    push {}", Register::Rdx.for_size(ty.size()))?;
                }
            }
            Builtin::Equal => {
                compare!(a, b);
                writeln!(writer, "    sete al")?;
                writeln!(writer, "    movzx rax, al")?;
                writeln!(writer, "    push rax")?;
            }
            Builtin::Not => {
                writeln!(writer, "    popq rax")?;
                writeln!(writer, "    cmp rax, 0")?;
                writeln!(writer, "    sete al")?;
                writeln!(writer, "    movzx rax, al")?;
                writeln!(writer, "    push rax")?;
            }
            Builtin::NotEqual => {
                compare!(a, b);
                writeln!(writer, "    setne al")?;
                writeln!(writer, "    movzx rax, al")?;
                writeln!(writer, "    push rax")?;
            }
            Builtin::Lt => {
                compare!(a, b);
                writeln!(writer, "    seta al")?;
                writeln!(writer, "    movzx rax, al")?;
                writeln!(writer, "    push rax")?;
            }
            Builtin::Lte => {
                compare!(a, b);
                writeln!(writer, "    setnb al")?;
                writeln!(writer, "    movzx rax, al")?;
                writeln!(writer, "    push rax")?;
            }
            Builtin::Gt => {
                compare!(b, a);
                writeln!(writer, "    seta al")?;
                writeln!(writer, "    movzx rax, al")?;
                writeln!(writer, "    push rax")?;
            }
            Builtin::Gte => {
                compare!(b, a);
                writeln!(writer, "    setnb al")?;
                writeln!(writer, "    movzx rax, al")?;
                writeln!(writer, "    push rax")?;
            }
            Builtin::Dup(n) => {
                assert!(type_stack.len() >= n as _);
                let length: u32 = type_stack
                    .iter()
                    .rev()
                    .take(n as _)
                    .map(|t| t.padded_size())
                    .sum();
                // Attempt to reduce operations by using bigger mnemonics
                let (inst, count) = if length % 8 == 0 {
                    ("movsq", length / 8)
                } else if length % 4 == 0 {
                    ("movsd", length / 4)
                } else if length % 2 == 0 {
                    ("movsw", length / 2)
                } else {
                    ("movsb", length)
                };

                writeln!(writer, "    mov rsi, rsp")?; // src = rsp
                writeln!(writer, "    sub rsp, {}", length)?; // rsp -= length
                writeln!(writer, "    mov rdi, rsp")?; // dst = rsp
                writeln!(writer, "    mov rcx, {}", count)?;
                writeln!(writer, "    rep {}", inst)?;
            }
            Builtin::Swap => {
                // TODO: move values bigger than registers
                assert_eq!(type_stack[len - 2].padded_size(), 8);
                assert_eq!(type_stack[len - 1].padded_size(), 8);
                writeln!(writer, "    popq rax")?;
                writeln!(writer, "    popq rbx")?;
                writeln!(writer, "    push rax")?;
                writeln!(writer, "    push rbx")?;
            }
            Builtin::Drop => {
                let ty = type_stack.last().unwrap();
                ty.drop(writer)?;
            }
            Builtin::Load => {
                let ty = type_stack.last().unwrap();
                let Type::Pointer(inner) = ty else {
                    panic!("ty is pointer");
                };
                writeln!(writer, "    popq rax")?;
                let (inst, mnemonic) = match inner.size() {
                    1 if inner.is_signed() => ("movsx", "byte"),
                    1 => ("movzx", "byte"),
                    2 if inner.is_signed() => ("movsx", "word"),
                    2 => ("movzx", "word"),
                    4 => ("mov", "dword"),
                    8 => ("mov", "qword"),
                    _ => todo!("arbitrary pointer inner size"),
                };
                writeln!(writer, "    {} rax, {} [rax]", inst, mnemonic)?;
                writeln!(writer, "    pushq rax")?;
            }
            Builtin::Store => {
                let [ptr, value] = type_stack.last_chunk::<2>().unwrap();
                let Type::Pointer(inner) = ptr else {
                    panic!("ty is pointer");
                };
                assert_eq!(**inner, *value);
                writeln!(writer, "    popq rax")?; // pop value
                writeln!(writer, "    popq rbx")?; // pop pointer
                let (register, mnemonic) = match inner.size() {
                    1 => ("al", "byte"),
                    2 => ("ax", "word"),
                    4 => ("eax", "dword"),
                    8 => ("rax", "qword"),
                    _ => todo!("arbitrary pointer inner size"),
                };
                writeln!(writer, "    mov {} [rbx], {}", mnemonic, register)?;
            }
        }
        Ok(())
    }
}

impl Cast {
    pub fn compile_to<W: Write>(
        self,
        writer: &mut W,
        type_stack: &mut TypeStack,
    ) -> Result<(), CodeGenError> {
        let src = type_stack.last().unwrap().clone();
        self.type_check(type_stack)?;
        writeln!(writer, "    ; cast {} -> {}", src, self.target)?;
        match self.target {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::Integer => {
                if src.is_integer() {
                    // in C, `(unsigned int)((char) -1)` is `(unsigned int)((int) -1)` instead of the `255` that I'd expect...
                    // but, let's be consistent with that
                    src.pop_as(writer, Register::Rax, &self.target)?;
                    writeln!(writer, "    pushq rax")?;
                } else if src.is_float() {
                    match src {
                        Type::F32 => {
                            writeln!(writer, "    movss {}, [rsp]", FloatRegister::Xmm0)?;
                            writeln!(writer, "    add rsp, {}", src.padded_size())?;
                            writeln!(writer, "    xor rax, rax")?;
                            writeln!(
                                writer,
                                "    cvtss2si {}, {}",
                                Register::Rax.for_size(4),
                                FloatRegister::Xmm0
                            )?;
                        }
                        Type::F64 | Type::Float => {
                            writeln!(writer, "    movsd {}, [rsp]", FloatRegister::Xmm0)?;
                            writeln!(writer, "    add rsp, {}", src.padded_size())?;
                            writeln!(
                                writer,
                                "    cvtsd2si {}, {}",
                                Register::Rax.for_size(4),
                                FloatRegister::Xmm0
                            )?;
                        }
                        _ => unreachable!(),
                    }
                    writeln!(writer, "    pushq rax")?;
                }
            }
            Type::U8 | Type::U16 | Type::U32 | Type::U64 => {
                if src.is_integer() {
                    // in C, `(unsigned int)((char) -1)` is `(unsigned int)((int) -1)` instead of the `255` that I'd expect...
                    // but, let's be consistent with that
                    src.pop_as(writer, Register::Rax, &self.target)?;
                    writeln!(writer, "    pushq rax")?;
                } else if src.is_float() {
                    match src {
                        Type::F32 => {
                            writeln!(writer, "    movss {}, [rsp]", FloatRegister::Xmm0)?;
                            writeln!(writer, "    add rsp, {}", src.padded_size())?;
                            writeln!(
                                writer,
                                "    cvtss2si {}, {}",
                                Register::Rax.for_size(4),
                                FloatRegister::Xmm0
                            )?;
                            writeln!(writer, "    pushq rax")?;
                        }
                        Type::F64 | Type::Float => {
                            writeln!(writer, "    movsd {}, [rsp]", FloatRegister::Xmm0)?;
                            writeln!(writer, "    add rsp, {}", src.padded_size())?;
                            writeln!(
                                writer,
                                "    cvtsd2si {}, {}",
                                Register::Rax.for_size(4),
                                FloatRegister::Xmm0
                            )?;
                            writeln!(writer, "    pushq rax")?;
                        }
                        _ => unreachable!(),
                    }
                }
            }
            Type::F32 => {
                if src.is_integer() {
                    // TODO: I64 -> f32 how?
                    src.pop_as(writer, Register::Rax, &Type::I32)?;
                    writeln!(
                        writer,
                        "    cvtsi2ss {}, {}",
                        FloatRegister::Xmm0,
                        Register::Rax.for_size(4)
                    )?;
                    writeln!(writer, "    sub rsp, 8")?;
                    writeln!(writer, "    movss [rsp], xmm0")?;
                } else if src.is_float() {
                    match src {
                        Type::F32 => { /* nop */ }
                        Type::F64 => {
                            writeln!(writer, "cvtsd2ss xmm0, [rsp]")?;
                            writeln!(writer, "movss [rsp], xmm0")?;
                        }
                        _ => unreachable!(),
                    }
                }
            }
            Type::F64 => {
                if src.is_integer() {
                    // TODO: I64 -> f32 how?
                    src.pop_as(writer, Register::Rax, &Type::I32)?;
                    writeln!(
                        writer,
                        "    cvtsi2sd {}, {}",
                        FloatRegister::Xmm0,
                        Register::Rax.for_size(4)
                    )?;
                    writeln!(writer, "    sub rsp, 8")?;
                    writeln!(writer, "    movsd [rsp], xmm0")?;
                } else if src.is_float() {
                    match src {
                        Type::F32 => {
                            writeln!(writer, "cvtss2sd xmm0, [rsp]")?;
                            writeln!(writer, "movsd [rsp], xmm0")?;
                        }
                        Type::F64 => { /* nop */ }
                        _ => unreachable!(),
                    }
                }
            }
            Type::Float => { /* nop */ }
            Type::Pointer(_) | Type::VoidPointer => { /* nop */ }
            Type::FatPointer => todo!(),
            Type::Struct => todo!(),
            Type::Bool => {
                writeln!(writer, "    pop rax")?;
                writeln!(writer, "    cmp rax, 0")?;
                writeln!(writer, "    setne al")?;
                writeln!(writer, "    movzx rax, al")?;
                writeln!(writer, "    pushq rax")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Error, Diagnostic)]
pub enum CodeGenError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error(transparent)]
    #[diagnostic(transparent)]
    IrGenError(#[from] IrGenError),
    #[error("Module is missing entrypoint")]
    #[diagnostic(help("Create a function with a valid main signature."))]
    MissingMain,
    #[error("Invalid main function definition")]
    #[diagnostic(help("Use one of the following valid main signatures:\n    fn main;\n    fn main(u32 **i8);\n    fn main -> (i32);\n    fn main(u32 **i8) -> (i32);"))]
    InvalidMain {
        #[label = "this function"]
        span: Span,
    },
    #[error("Not yet implemented: {}", feature)]
    NotImplemented {
        #[label = "here"]
        span: Span,
        feature: String,
    },
}

macro_rules! nyi {
    ($span: expr, $feature: literal) => {
        return Err(CodeGenError::NotImplemented {
            span: $span,
            feature: $feature.into(),
        })
    };
}

pub struct CodeGen<W> {
    module: Module,
    data: DataItems,
    type_stack: TypeStack,
    writer: W,
}

fn get_all_functions<'a>(
    module: &'a Module,
    out: &mut HashMap<Vec<String>, &'a FunctionSignature>,
) {
    for (name, s) in module.functions.iter() {
        let mut v = module.path.clone();
        v.push(name.clone());
        out.insert(v, s);
    }
    for (m, _vis) in module.submodules.values() {
        get_all_functions(m, out);
    }
}

fn get_all_converted_functions(
    module: &mut Module,
    out: &mut HashMap<Vec<String>, ConvertedFunction>,
) {
    for f in module.converted_functions.drain(..) {
        let mut v = module.path.clone();
        v.push(f.name.clone());
        out.insert(v, f);
    }
    for (m, _vis) in module.submodules.values_mut() {
        get_all_converted_functions(m, out);
    }
}

impl<W: Write> CodeGen<W> {
    pub fn new(module: Module, writer: W) -> Self {
        Self {
            module,
            writer,
            type_stack: TypeStack::new(),
            data: Default::default(),
        }
    }

    pub fn compile(&mut self) -> Result<(), CodeGenError> {
        let mut converted_functions = HashMap::new();
        get_all_converted_functions(&mut self.module, &mut converted_functions);

        writeln!(self.writer, r#"format ELF64"#)?;
        writeln!(self.writer, r#"section ".text" executable"#)?;

        writeln!(
            self.writer,
            r#"public ?{}$main as "main""#,
            self.module.path.join("$")
        )?;
        writeln!(self.writer)?;

        let mut functions = HashMap::new();
        get_all_functions(&self.module, &mut functions);
        for (path, f) in functions {
            if f.external {
                writeln!(
                    self.writer,
                    "extrn {:?} as ?{}",
                    f.linker_name,
                    path.join("$")
                )?;
            }
        }
        writeln!(self.writer)?;

        for (path, f) in converted_functions {
            self.type_stack.clear();
            let (signature, module) = self.module.resolve_path(&path[1..]).unwrap();
            self.type_stack.extend_from_slice(&signature.args);
            let is_main = module.path == self.module.path && path[1] == "main";
            self.compile_function(path, f, is_main)?;
        }

        // self.compile_main_function(converted_functions)?;

        self.data.write(&mut self.writer)?;
        Ok(())
    }

    fn compile_main_function(
        &mut self,
        mut fns: Vec<ConvertedFunction>,
    ) -> Result<(), CodeGenError> {
        let main = self
            .module
            .functions
            .get("main")
            .ok_or(CodeGenError::MissingMain)?;
        let main_ir = fns.pop().unwrap();

        if !main.args.is_empty()
            && !main
                .args
                .matches(&TypeStack::from(vec![ty!(u32), ty!(**i8)]))
        {
            return Err(CodeGenError::InvalidMain {
                span: main.ident_span,
            });
        }

        if !main.returns.is_empty() && !main.returns.matches(&TypeStack::from(vec![ty!(i32)])) {
            return Err(CodeGenError::InvalidMain {
                span: main.ident_span,
            });
        }

        // TODO: self.compile_function(main_ir, true)?;

        Ok(())
    }

    fn compile_function(
        &mut self,
        path: Vec<String>,
        f: ConvertedFunction,
        // this is a very very very dirty hack....
        // TODO: C/Linux calling convention
        is_main: bool,
    ) -> Result<(), CodeGenError> {
        let (func, module) = self.module.resolve_path(&path[1..]).unwrap();
        let module = module.light_clone();

        self.type_stack.clear();
        self.type_stack.extend_from_slice(&func.args);

        writeln!(self.writer, "?{}:", path.join("$"))?;

        if is_main {
            writeln!(self.writer, "    push rbp")?;
            writeln!(self.writer, "    mov rbp, rsp")?;
        } else {
            let floati = func.args.iter().filter(|t| t.is_float()).count();
            let regi = func
                .args
                .iter()
                .filter(|t| !t.is_float() && t.size() <= 8)
                .count();
            if floati >= FloatRegister::ARG_REGS.len()
                || regi >= Register::ARG_REGS.len()
                || floati + regi != func.args.len()
            {
                todo!("extra values on the stack");
            }
            let mut floati = floati.saturating_sub(1);
            let mut regi = regi.saturating_sub(1);
            for x in func.args.iter().rev() {
                if x.is_float() {
                    writeln!(self.writer, "    sub rsp, {}", x.padded_size())?;
                    writeln!(
                        self.writer,
                        "    movss [rsp], {}",
                        FloatRegister::ARG_REGS[floati]
                    )?;
                    floati = floati.saturating_sub(1);
                } else if x.size() <= 8 {
                    writeln!(
                        self.writer,
                        "    push {}",
                        Register::ARG_REGS[regi].for_size(8)
                    )?;
                    regi = regi.saturating_sub(1);
                }
            }
            // writeln!(self.writer, "    push rbp")?;
            // writeln!(self.writer, "    mov rbp, rsp")?;

            // writeln!(self.writer, "    mov rax, [rsp]")?; // save the return pointer
            // writeln!(self.writer)?;
            // writeln!(self.writer, "    mov rdi, rsp")?; // rdi is the destination for the `movsq` instruction
            // writeln!(self.writer, "    mov rsi, rsp")?; // rsi is the source for the `movsq` instruction
            // writeln!(self.writer, "    add rsi, 8")?; // rsi+8 to shift everything down
            // writeln!(self.writer)?;
            // writeln!(self.writer, "    mov rcx, {}", func.args.len())?; // repeat for each arg
            // writeln!(self.writer, "    cld")?; // clear direction flag so `rep` increments
            // writeln!(self.writer, "    rep movsq")?;
            // // save the return address on the bottom of the stack
            // writeln!(self.writer, "    mov [rsp+{}], rax", func.args.len() * 8)?;
        }

        dbg!(&module.path, f.name);
        self.compile_body(f.body, &module)?;
        let (func, _module) = &self.module.resolve_path(&path[1..]).unwrap();

        if is_main {
            writeln!(self.writer, "    mov rax, 0")?;
            writeln!(self.writer, "    pop rbp")?;
        }

        let mut returns = func.returns.clone();
        if let Some(top) = returns.deref_mut().pop() {
            if top.is_float() {
                if top == Type::F32 {
                    writeln!(self.writer, "    movss xmm0, [rsp]")?;
                    writeln!(self.writer, "    add rsp, 8")?;
                } else {
                    writeln!(self.writer, "    movsd xmm0")?;
                    writeln!(self.writer, "    add rsp, 8")?;
                }
            } else if top.size() <= 8 {
                writeln!(self.writer, "    popq rax")?;
            } else {
                todo!("extra values on the stack");
            }
        }

        if !returns.is_empty() {
            writeln!(self.writer)?;
            writeln!(
                self.writer,
                "    mov r8, [rsp+{}]",
                returns.iter().map(|t| t.padded_size()).sum::<u32>()
            )?; // get the return address from the bottom of the stack
            writeln!(self.writer)?;
            writeln!(self.writer, "    mov rsi, rsp")?;
            writeln!(self.writer, "    add rsi, {}", (returns.len() - 1) * 8)?;
            writeln!(self.writer, "    mov rdi, rsp")?;
            writeln!(self.writer, "    add rdi, {}", returns.len() * 8)?;
            writeln!(self.writer)?;
            writeln!(self.writer, "    std")?; // set direction flag so `rep` decrements
            writeln!(self.writer, "    mov rcx, {}", returns.len())?;
            writeln!(self.writer, "    rep movsq")?;
            writeln!(self.writer)?;
            writeln!(self.writer, "    mov [rsp], r8")?;
        }

        writeln!(self.writer, "    ret")?;
        writeln!(self.writer)?;

        Ok(())
    }

    fn compile_body(&mut self, body: Vec<Ir>, module: &Module) -> Result<(), CodeGenError> {
        for ir in body {
            self.compile_ir(ir, module)?;
        }
        Ok(())
    }

    fn compile_ir(&mut self, ir: Ir, module: &Module) -> Result<(), CodeGenError> {
        match ir.kind {
            IrKind::PushInt(n, ty) => {
                self.type_stack.push(ty.into());
                writeln!(self.writer, "    mov rax, {}", n)?;
                writeln!(self.writer, "    pushq rax")?;
            }
            IrKind::PushFloat(f, ty) => {
                self.type_stack.push(ty.into());
                writeln!(
                    self.writer,
                    "    sub rsp, {}",
                    Type::from(&ty).padded_size()
                )?;
                match ty {
                    FloatLitType::F32 => {
                        let label = self.data.add_f32(f as f32);
                        writeln!(self.writer, "    movss xmm0, [{}]", label)?;
                        writeln!(self.writer, "    movss [rsp], xmm0")?;
                    }
                    FloatLitType::F64 | FloatLitType::Unresolved => {
                        let label = self.data.add_f64(f);
                        writeln!(self.writer, "    movsd xmm0, [{}]", label)?;
                        writeln!(self.writer, "    movsd [rsp], xmm0")?;
                    }
                }
            }
            IrKind::PushBool(b) => {
                self.type_stack.push(ty!(bool));
                writeln!(self.writer, "    mov rax, {}", if b { 1 } else { 0 })?;
                writeln!(self.writer, "    pushq rax")?;
            }
            IrKind::PushCStr(s) => {
                self.type_stack.push(ty!(*i8));
                let label = self.data.add_bytes(s.to_bytes_with_nul().into());
                writeln!(self.writer, "    push {}", label)?;
            }
            IrKind::PushStr(_) => {
                nyi!(ir.span, "Fat pointers");
                // let label = self.data.add_bytes(s.as_bytes().into());
                // TODO: fat pointers
                // writeln!(self.writer, "    push {}", label)?;
            }
            IrKind::CallFn(ident) => {
                self.compile_function_call(ident, module)?;
            }
            IrKind::CallBuiltin(builtin) => {
                let (consumed, returns) = builtin.type_check(ir.span, &self.type_stack)?;
                builtin.compile_to(&mut self.writer, &mut self.type_stack)?;
                let len = self.type_stack.len();
                self.type_stack.truncate(len - consumed);
                self.type_stack.extend_from_slice(&returns);
            }
            IrKind::Then(then) => self.compile_then(then, module)?,
            IrKind::While(whil) => self.compile_while(whil, module)?,
            IrKind::Break(id) => {
                writeln!(self.writer, "    jmp .{}_break", id)?;
            }
            IrKind::Cast(cast) => {
                cast.compile_to(&mut self.writer, &mut self.type_stack)?;
            }
        }
        Ok(())
    }

    fn compile_then(&mut self, then: ThenIr, module: &Module) -> Result<(), CodeGenError> {
        let id = then.then_span.start();
        self.type_stack.pop_type(then.then_span, &ty!(bool))?;

        writeln!(self.writer, "    popq rax")?;
        writeln!(self.writer, "    cmp rax, 0")?;
        if then.elze.is_empty() {
            writeln!(self.writer, "    je .then_end_{}", id)?;
        } else {
            writeln!(self.writer, "    je .else_{}_{}", id, 0)?;
        }

        let start = self.type_stack.clone();
        self.compile_body(then.body, module)?;
        let mut after_then = self.type_stack.clone();
        writeln!(self.writer, "    jmp .then_end_{}", id)?;

        let et_len = then.else_thens.len();
        for (i, et) in then.else_thens.into_iter().enumerate() {
            let _ = std::mem::replace(&mut self.type_stack, start.clone());
            writeln!(self.writer, ".else_{}_{}:", id, i)?;
            self.compile_body(et.condition, module)?;
            self.type_stack.pop_type(then.then_span, &Type::Bool)?;
            writeln!(self.writer, "    popq rax")?;
            writeln!(self.writer, "    cmp rax, 0")?;

            if i == et_len - 1 && then.elze.is_empty() {
                writeln!(self.writer, "    je .then_end_{}", id)?;
            } else {
                writeln!(self.writer, "    je .else_{}_{}", id, i + 1)?;
            }

            self.compile_body(et.body, module)?;
            writeln!(self.writer, "    jmp .then_end_{}", id)?;
        }

        if !then.elze.is_empty() {
            let _ = std::mem::replace(&mut self.type_stack, start.clone());
            writeln!(self.writer, ".else_{}_{}:", id, et_len)?;
            self.compile_body(then.elze, module)?;
        }
        std::mem::swap(&mut self.type_stack, &mut after_then);
        writeln!(self.writer, ".then_end_{}:", id)?;

        Ok(())
    }

    fn compile_while(&mut self, whil: WhileIr, module: &Module) -> Result<(), CodeGenError> {
        let id = whil.loop_id;
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

        writeln!(self.writer, "    jmp .{}_end", id)?;
        writeln!(self.writer, ".{}:", id)?;
        self.compile_body(whil.body, module)?;
        assert!(start.matches(&self.type_stack));
        writeln!(self.writer, ".{}_end:", id)?;
        self.compile_body(whil.condition, module)?;
        self.type_stack.pop_type(whil.condition_span, &Type::Bool)?;
        writeln!(self.writer, "    popq rax")?;
        writeln!(self.writer, "    cmp rax, 0")?;
        writeln!(self.writer, "    jne .{}", id)?;
        writeln!(self.writer, ".{}_break:", id)?;
        assert!(start.matches(&self.type_stack));

        Ok(())
    }

    fn compile_function_call(&mut self, ident: Ident, module: &Module) -> Result<(), CodeGenError> {
        let f = module.resolve_ident(&ident.path)?;

        let mut register_i = 0;
        let mut float_i = 0;
        let mut argi = 0;
        for _ in 0..ident.arity.map(|x| x as usize).unwrap_or(f.args.len()) {
            let ty = self.type_stack.pop(ident.span())?;
            let arg_ty = if argi < f.args.len() {
                let arg_ty = &f.args[f.args.len() - argi - 1];
                if argi < f.args.len() {
                    assert!(
                        ty.matches(arg_ty),
                        "Mismatched types.  Expected: {},  Actual: {}",
                        f.args[argi],
                        ty
                    );
                    argi += 1;
                }
                Some(arg_ty)
            } else {
                assert!(f.variadic);
                None
            };
            let dest = ty.argument_dest();
            match dest {
                ArgumentDest::Register if register_i < Register::ARG_REGS.len() => {
                    let dest = Register::ARG_REGS[register_i];
                    writeln!(self.writer, "    popq {}", dest.for_size(8))?;
                    register_i += 1;
                }
                ArgumentDest::Register => todo!("overflow on stack"),
                ArgumentDest::Stack => todo!("compact on the stack"),
                ArgumentDest::FloatRegister if float_i < FloatRegister::ARG_REGS.len() => {
                    let dest = FloatRegister::ARG_REGS[float_i];
                    writeln!(self.writer, "    pxor {0}, {0}", dest)?;
                    let instruction = match (ty, arg_ty) {
                        (Type::F32, Some(Type::F32)) => "movss",
                        (Type::F32, Some(Type::F64) | None) => "cvtss2sd",
                        (Type::F64 | Type::Float, Some(Type::F32)) => "cvtsd2ss",
                        (Type::F64 | Type::Float, Some(Type::F64)) => "movsd",
                        _ => unreachable!(),
                    };
                    writeln!(self.writer, "    {} {}, [rsp]", instruction, dest)?;
                    writeln!(self.writer, "    add rsp, 8")?;
                    float_i += 1;
                }
                ArgumentDest::FloatRegister => todo!("overflow on stack"),
            }
        }

        self.type_stack.extend_from_slice(&f.returns);
        // TODO: What should go in rax?
        writeln!(self.writer, "    mov rax, {}", float_i)?;

        dbg!((&module.path, &f.name));
        writeln!(
            self.writer,
            "    call ?{}${}",
            module.path.join("$"),
            ident
                .path
                .iter()
                .map(|p| p.name.clone())
                .collect::<Vec<_>>()
                .join("$")
        )?;

        // Push first argument back onto the stack
        if let Some(ret) = f.returns.first() {
            match ret.argument_dest() {
                ArgumentDest::Register => writeln!(self.writer, "    push rax")?,
                ArgumentDest::Stack => todo!(),
                ArgumentDest::FloatRegister => {
                    assert_eq!(*ret, ty!(f32));
                    writeln!(self.writer, "    sub rsp, 8")?;
                    writeln!(self.writer, "    movss [rsp], xmm0")?;
                }
            }
        }

        Ok(())
    }
}
