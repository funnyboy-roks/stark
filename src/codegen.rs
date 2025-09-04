use std::{borrow::Cow, collections::HashMap, fmt::Display, io::Write};

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::{
    hash_float::FloatExt,
    ir::{Builtin, ConvertedFunction, Ir, IrGenError, IrKind, Module, Type, TypeStack},
    parse::Ident,
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
            Type::Pointer(_) => ArgumentDest::Register,
            Type::FatPointer => todo!(),
            Type::Struct => todo!(),
            Type::Bool => ArgumentDest::Register,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct DataItems {
    bytes: HashMap<Box<[u8]>, usize>,
    floats: HashMap<FloatExt<f64>, usize>,
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

    pub fn add_float(&mut self, float: f64) -> String {
        let len = self.floats.len();
        let n = self.floats.entry(float.into()).or_insert(len);
        format!("float_{}", n)
    }

    pub fn write(&self, mut out: impl Write) -> Result<(), CodeGenError> {
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

impl Type {
    pub fn drop<W: Write>(&self, writer: &mut W) -> Result<(), CodeGenError> {
        if self.is_float() {
            writeln!(writer, "    add rsp, 4")?;
        } else {
            writeln!(writer, "    add rsp, {}", self.padded_size())?;
        }
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
        match self {
            Builtin::Add => todo!(),
            Builtin::Sub => todo!(),
            Builtin::Mul => todo!(),
            Builtin::Div => todo!(),
            Builtin::Equal => {
                if type_stack[len - 1].is_float() {
                    assert_eq!(type_stack[len - 2].padded_size(), 4);
                    assert_eq!(type_stack[len - 1].padded_size(), 4);
                    writeln!(writer, "    movss {}, [rsp]", FloatRegister::Xmm0)?;
                    writeln!(writer, "    movss {}, [rsp+4]", FloatRegister::Xmm1)?;
                    writeln!(writer, "    add rsp, 8")?;
                    writeln!(
                        writer,
                        "    comiss {}, {}",
                        FloatRegister::Xmm0,
                        FloatRegister::Xmm1,
                    )?;
                } else {
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert!(type_stack[len - 1].is_integer());
                    writeln!(writer, "    popq rax")?;
                    writeln!(writer, "    popq rbx")?;
                    writeln!(writer, "    cmp rbx, rax")?;
                }
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
                if type_stack[len - 1].is_float() {
                    assert_eq!(type_stack[len - 2].padded_size(), 4);
                    assert_eq!(type_stack[len - 1].padded_size(), 4);
                    writeln!(writer, "    movss {}, [rsp]", FloatRegister::Xmm0)?;
                    writeln!(writer, "    movss {}, [rsp+4]", FloatRegister::Xmm1)?;
                    writeln!(writer, "    add rsp, 8")?;
                    writeln!(
                        writer,
                        "    comiss {}, {}",
                        FloatRegister::Xmm0,
                        FloatRegister::Xmm1,
                    )?;
                } else {
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert!(type_stack[len - 1].is_integer());
                    writeln!(writer, "    popq rax")?;
                    writeln!(writer, "    popq rbx")?;
                    writeln!(writer, "    cmp rbx, rax")?;
                }
                writeln!(writer, "    setne al")?;
                writeln!(writer, "    movzx rax, al")?;
                writeln!(writer, "    push rax")?;
            }
            Builtin::Lt => {
                if type_stack[len - 1].is_float() {
                    assert_eq!(type_stack[len - 1].padded_size(), 4);
                    assert_eq!(type_stack[len - 2].padded_size(), 4);
                    writeln!(writer, "    movss {}, [rsp]", FloatRegister::Xmm0)?;
                    writeln!(writer, "    movss {}, [rsp+4]", FloatRegister::Xmm1)?;
                    writeln!(writer, "    add rsp, 8")?;
                    writeln!(
                        writer,
                        "    comiss {}, {}",
                        FloatRegister::Xmm0,
                        FloatRegister::Xmm1,
                    )?;

                    writeln!(writer, "    seta al")?;
                } else {
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    assert!(type_stack[len - 1].is_integer());
                    writeln!(writer, "    popq rax")?;
                    writeln!(writer, "    popq rbx")?;
                    writeln!(writer, "    cmp rax, rbx")?;
                }
                writeln!(writer, "    seta al")?;
                writeln!(writer, "    movzx rax, al")?;
                writeln!(writer, "    push rax")?;
            }
            Builtin::Lte => {
                if type_stack[len - 1].is_float() {
                    assert_eq!(type_stack[len - 1].padded_size(), 4);
                    assert_eq!(type_stack[len - 2].padded_size(), 4);
                    writeln!(writer, "    movss {}, [rsp]", FloatRegister::Xmm0)?;
                    writeln!(writer, "    movss {}, [rsp+4]", FloatRegister::Xmm1)?;
                    writeln!(writer, "    add rsp, 8")?;
                    writeln!(
                        writer,
                        "    comiss {}, {}",
                        FloatRegister::Xmm0,
                        FloatRegister::Xmm1,
                    )?;

                    writeln!(writer, "    seta al")?;
                } else {
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    assert!(type_stack[len - 1].is_integer());
                    writeln!(writer, "    popq rax")?;
                    writeln!(writer, "    popq rbx")?;
                    writeln!(writer, "    cmp rax, rbx")?;
                }
                writeln!(writer, "    setnb al")?;
                writeln!(writer, "    movzx rax, al")?;
                writeln!(writer, "    push rax")?;
            }
            Builtin::Gt => {
                if type_stack[len - 1].is_float() {
                    assert_eq!(type_stack[len - 1].padded_size(), 4);
                    assert_eq!(type_stack[len - 2].padded_size(), 4);
                    writeln!(writer, "    movss {}, [rsp]", FloatRegister::Xmm0)?;
                    writeln!(writer, "    movss {}, [rsp+4]", FloatRegister::Xmm1)?;
                    writeln!(writer, "    add rsp, 8")?;
                    writeln!(
                        writer,
                        "    comiss {}, {}",
                        FloatRegister::Xmm1,
                        FloatRegister::Xmm0,
                    )?;

                    writeln!(writer, "    seta al")?;
                } else {
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    assert!(type_stack[len - 1].is_integer());
                    writeln!(writer, "    popq rax")?;
                    writeln!(writer, "    popq rbx")?;
                    writeln!(writer, "    cmp rbx, rax")?;
                }
                writeln!(writer, "    seta al")?;
                writeln!(writer, "    movzx rax, al")?;
                writeln!(writer, "    push rax")?;
            }
            Builtin::Gte => {
                if type_stack[len - 1].is_float() {
                    assert_eq!(type_stack[len - 1].padded_size(), 4);
                    assert_eq!(type_stack[len - 2].padded_size(), 4);
                    writeln!(writer, "    movss {}, [rsp]", FloatRegister::Xmm0)?;
                    writeln!(writer, "    movss {}, [rsp+4]", FloatRegister::Xmm1)?;
                    writeln!(writer, "    add rsp, 8")?;
                    writeln!(
                        writer,
                        "    comiss {}, {}",
                        FloatRegister::Xmm1,
                        FloatRegister::Xmm0,
                    )?;

                    writeln!(writer, "    seta al")?;
                } else {
                    assert_eq!(type_stack[len - 1].padded_size(), 8);
                    assert_eq!(type_stack[len - 2].padded_size(), 8);
                    assert!(type_stack[len - 1].is_integer());
                    writeln!(writer, "    popq rax")?;
                    writeln!(writer, "    popq rbx")?;
                    writeln!(writer, "    cmp rbx, rax")?;
                }
                writeln!(writer, "    setnb al")?;
                writeln!(writer, "    movzx rax, al")?;
                writeln!(writer, "    push rax")?;
            }
            Builtin::Mod => todo!(),
            Builtin::Dup => {
                // TODO: move values bigger than registers
                assert_eq!(type_stack[len - 1].padded_size(), 8);
                writeln!(writer, "    popq rax")?;
                writeln!(writer, "    push rax")?;
                writeln!(writer, "    push rax")?;
            }
            Builtin::Dup2 => {
                // TODO: move values bigger than registers
                assert_eq!(type_stack[len - 1].padded_size(), 8);
                assert_eq!(type_stack[len - 2].padded_size(), 8);
                writeln!(writer, "    popq rax")?;
                writeln!(writer, "    popq rbx")?;

                writeln!(writer, "    push rbx")?;
                writeln!(writer, "    push rax")?;
                writeln!(writer, "    push rbx")?;
                writeln!(writer, "    push rax")?;
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
        }
        Ok(())
    }
}

#[derive(Debug, Error, Diagnostic)]
pub enum CodeGenError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error(transparent)]
    IrGenError(#[from] IrGenError),
    #[error("Module is missing entrypoint")]
    #[diagnostic(help("Create a function with a valid main signature."))]
    MissingMain,
    #[error("Invalid main function definition")]
    #[diagnostic(help("Use one of the following valid main signatures:\n    fn main;\n    fn main(u32 **i8);\n    fn main -> (i32);\n    fn main(u32 **i8) -> (i32);"))]
    InvalidMain {
        #[label = "this function"]
        span: SourceSpan,
    },
    #[error("Not yet implemented: {}", feature)]
    NotImplemented {
        #[label = "here"]
        span: SourceSpan,
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
        let mut converted_functions = std::mem::take(&mut self.module.converted_functions);

        writeln!(self.writer, r#"format ELF64"#)?;
        writeln!(self.writer, r#"section ".text" executable"#)?;

        writeln!(self.writer, r#"public main"#)?;
        writeln!(self.writer)?;

        for f in self.module.functions.values() {
            if f.external {
                writeln!(self.writer, "extrn {}", f.linker_name)?;
            }
        }
        writeln!(self.writer)?;

        for f in converted_functions.extract_if(.., |f| f.linker_name != "main") {
            self.compile_function(f, false)?;
        }

        self.compile_main_function(converted_functions)?;

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

        self.compile_function(main_ir, true)?;

        Ok(())
    }

    fn compile_function(
        &mut self,
        f: ConvertedFunction,
        // this is a very very very dirty hack....
        // TODO: C/Linux calling convention
        is_main: bool,
    ) -> Result<(), CodeGenError> {
        let func = &self.module.functions[&f.name];

        self.type_stack.clear();
        self.type_stack.extend_from_slice(&func.args);

        writeln!(self.writer, "{}:", f.linker_name)?;
        if is_main {
            writeln!(self.writer, "    push rbp")?;
            writeln!(self.writer, "    mov rbp, rsp")?;
        }

        for ir in f.body {
            self.compile_ir(ir)?;
        }

        if is_main {
            writeln!(self.writer, "    mov rax, 0")?;
            writeln!(self.writer, "    pop rbp")?;
        }

        writeln!(self.writer, "    ret")?;
        writeln!(self.writer)?;

        Ok(())
    }

    fn compile_ir(&mut self, ir: Ir) -> Result<(), CodeGenError> {
        match ir.kind {
            IrKind::PushInt(n) => {
                self.type_stack.push(ty!(Integer));
                writeln!(self.writer, "    mov rax, {}", n)?;
                writeln!(self.writer, "    pushq rax")?;
            }
            IrKind::PushFloat(f) => {
                let label = self.data.add_float(f);
                self.type_stack.push(ty!(Float));
                writeln!(self.writer, "    movss xmm0, [{}]", label)?;
                writeln!(self.writer, "    sub rsp, 4")?;
                writeln!(self.writer, "    movss [rsp], xmm0")?;
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
                self.compile_function_call(ident)?;
            }
            IrKind::CallBuiltin(builtin) => {
                let (consumed, returns) = builtin.type_check(ir.span, &self.type_stack)?;
                builtin.compile_to(&mut self.writer, &mut self.type_stack)?;
                let len = self.type_stack.len();
                self.type_stack.truncate(len - consumed);
                self.type_stack.extend_from_slice(&returns);
            }
            IrKind::Then(_) => nyi!(ir.span, "then/else-then/else"),
            IrKind::While(_) => nyi!(ir.span, "while loops"),
            IrKind::Break(_) => nyi!(ir.span, "break statements"),
            IrKind::Cast(_) => nyi!(ir.span, "casting"),
        }
        Ok(())
    }

    fn compile_function_call(&mut self, ident: Ident) -> Result<(), CodeGenError> {
        let f = &self.module.functions[&ident.ident];

        if f.external {
            let mut register_i = 0;
            let mut float_i = 0;
            let mut argi = 0;
            for _ in 0..ident.len.map(|x| x as usize).unwrap_or(f.args.len()) {
                let ty = self.type_stack.pop(ident.span)?;
                if argi < f.args.len() {
                    assert_eq!(ty, f.args[argi]);
                    argi += 1;
                }
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
                        let dest = FloatRegister::ARG_REGS[register_i];
                        writeln!(self.writer, "    movss [rsp], {}", dest)?;
                        writeln!(self.writer, "    add rsp, 4")?;
                        float_i += 1;
                    }
                    ArgumentDest::FloatRegister => todo!("overflow on stack"),
                }
            }

            self.type_stack.extend_from_slice(&f.returns);
            writeln!(self.writer, "    mov rax, 0")?;
            writeln!(self.writer, "    call {}", f.linker_name)?;

            // Push first argument back onto the stack
            if let Some(ret) = f.returns.first() {
                match ret.argument_dest() {
                    ArgumentDest::Register => writeln!(self.writer, "    push rax")?,
                    ArgumentDest::Stack => todo!(),
                    ArgumentDest::FloatRegister => {
                        assert_eq!(*ret, ty!(f32));
                        writeln!(self.writer, "    sub rsp, 4")?;
                        writeln!(self.writer, "    movss [rsp], xmm0")?;
                    }
                }
            }
        } else {
            f.apply(&ident, &mut self.type_stack)?;
            writeln!(self.writer, "    mov rax, 0")?;
            writeln!(self.writer, "    call {}", f.linker_name)?;
        }

        // for t in f.returns {
        //     self.type_stack.push(Type::from_atom(&t).expect("TODO"));
        // }

        Ok(())
    }
}
