use std::{
    collections::HashMap,
    ffi::CString,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
    path::PathBuf,
    rc::Rc,
};

use miette::{Diagnostic, NamedSource};
use thiserror::Error;

use crate::{
    lex::{Lexer, NumLitVal},
    parse::{
        Ast, AtomKind, Ident, ImportTree, ImportTreeValue, ModuleDeclaration, Parser, Path,
        PathElement, TypeAtom, Visibility,
    },
    span::{Span, Spanned},
};

#[derive(Debug, Error, Diagnostic)]
pub enum IrGenError {
    #[error("Stack underflow")]
    StackUnderflow {
        #[label = "because of this"]
        span: Span,
    },
    #[error("Type Error")]
    TypeError2 {
        #[label = "{message}"]
        span: Span,
        message: String,
    },
    #[error("'{ident}' defined multiple times")]
    RepeatDefinition {
        ident: String,
        #[label = "original definition here"]
        original: Span,
        #[label = "repeat definition here"]
        repeat: Span,
    },
    #[error("Explicit arg size is incorrect.  Expected: {expected}  Found: {actual}")]
    #[diagnostic(help("You may omit arg size for functions which are not variadic."))]
    IncorrectExplicitArgSize {
        expected: usize,
        actual: usize,
        #[label = "here"]
        span: Span,
    },

    #[error("Variadic arg size is incorrect.  Expected at least: {expected}  Found: {actual}")]
    #[diagnostic(help(
        "When specifying no variadic args, you may omit the arg size for the function."
    ))]
    IncorrectExplicitVariadicArgSize {
        expected: usize,
        actual: usize,
        #[label = "here"]
        span: Span,
    },
    #[error("Unknown submodule '{name}'")]
    UnknownSubmodule {
        name: String,
        #[label = "here"]
        span: Span,
    },
    #[error("Undefined symbol '{ident}'")]
    UndefinedSymbol {
        ident: String,
        #[label = "here"]
        span: Span,
    },
    #[error("Unexpected module reference.  Expected a function reference.")]
    UnexpectedModule {
        #[label = "here"]
        span: Span,
    },
    #[error("Cannot find file for module '{name}', checked paths: {checked_paths:?}")]
    MissingModuleFile {
        name: String,
        #[label = "this declaration"]
        span: Span,
        checked_paths: Vec<PathBuf>,
    },
    #[error("Error while reading module file {}: {}", path.display(), source)]
    ModuleReadError {
        source: std::io::Error,
        #[label = "this module"]
        span: Span,
        path: PathBuf,
    },
    #[error("{}", .0)]
    #[diagnostic(transparent)]
    ModuleParseError(miette::Report),
    #[error("{}", .0)]
    #[diagnostic(transparent)]
    ModuleIrGenError(miette::Report),
    #[error("Attempt to access private function '{name}'")]
    PrivateFunction {
        name: String,
        #[label = "here"]
        span: Span,
    },
    #[error("Attempt to access private module '{name}'")]
    PrivateModule {
        name: String,
        #[label = "here"]
        span: Span,
    },
    #[error("Stack changed within block.  Before: {before:?}  After: {after:?}")]
    StackChanged {
        before: TypeStack,
        after: TypeStack,
        #[label = "in this block"]
        span: Span,
    },

    #[error("Incorrect stack result in function.  Expected: {expected:?}  Actual: {actual:?}")]
    IncorrectStackResults {
        expected: TypeStack,
        actual: TypeStack,
        #[label = "in this function"]
        span: Span,
    },
}

fn wrap_miette<T>(
    r: Result<T, impl Into<miette::Error>>,
    path: &std::path::Path,
    content: impl Into<String>,
) -> Result<T, miette::Report> {
    r.map_err(|e| {
        e.into().with_source_code(
            NamedSource::new(path.to_string_lossy(), content.into()).with_language("Rust"),
        )
    })
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    /// Unresolved integer type
    Integer,
    F32,
    F64,
    /// Unresolved float type
    Float,
    Pointer(Box<Type>),
    /// `*void`
    VoidPointer,
    // TODO: make this work
    FatPointer,
    // TODO: make this work
    Struct,
    Bool,
}

// TODO: hold the type here
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum IntLitType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Unresolved,
}

impl From<IntLitType> for Type {
    fn from(value: IntLitType) -> Self {
        match value {
            IntLitType::I8 => Type::I8,
            IntLitType::I16 => Type::I16,
            IntLitType::I32 => Type::I32,
            IntLitType::I64 => Type::I64,
            IntLitType::U8 => Type::U8,
            IntLitType::U16 => Type::U16,
            IntLitType::U32 => Type::U32,
            IntLitType::U64 => Type::U64,
            IntLitType::Unresolved => Type::Integer,
        }
    }
}

impl From<&IntLitType> for Type {
    fn from(value: &IntLitType) -> Self {
        match value {
            IntLitType::I8 => Type::I8,
            IntLitType::I16 => Type::I16,
            IntLitType::I32 => Type::I32,
            IntLitType::I64 => Type::I64,
            IntLitType::U8 => Type::U8,
            IntLitType::U16 => Type::U16,
            IntLitType::U32 => Type::U32,
            IntLitType::U64 => Type::U64,
            IntLitType::Unresolved => Type::Integer,
        }
    }
}

impl From<&Type> for IntLitType {
    fn from(value: &Type) -> Self {
        match value {
            Type::I8 => Self::I8,
            Type::I16 => Self::I16,
            Type::I32 => Self::I32,
            Type::I64 => Self::I64,
            Type::U8 => Self::U8,
            Type::U16 => Self::U16,
            Type::U32 => Self::U32,
            Type::U64 => Self::U64,
            Type::Integer => Self::Unresolved,
            _ => unreachable!(),
        }
    }
}

impl From<Type> for IntLitType {
    fn from(value: Type) -> Self {
        match value {
            Type::I8 => Self::I8,
            Type::I16 => Self::I16,
            Type::I32 => Self::I32,
            Type::I64 => Self::I64,
            Type::U8 => Self::U8,
            Type::U16 => Self::U16,
            Type::U32 => Self::U32,
            Type::U64 => Self::U64,
            Type::Integer => Self::Unresolved,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum FloatLitType {
    F32,
    F64,
    Unresolved,
}

impl From<FloatLitType> for Type {
    fn from(value: FloatLitType) -> Self {
        match value {
            FloatLitType::F32 => Type::F32,
            FloatLitType::F64 => Type::F64,
            FloatLitType::Unresolved => Type::Float,
        }
    }
}

impl From<&FloatLitType> for Type {
    fn from(value: &FloatLitType) -> Self {
        match value {
            FloatLitType::F32 => Type::F32,
            FloatLitType::F64 => Type::F64,
            FloatLitType::Unresolved => Type::Float,
        }
    }
}

impl From<Type> for FloatLitType {
    fn from(value: Type) -> Self {
        match value {
            Type::F32 => Self::F32,
            Type::F64 => Self::F64,
            Type::Float => Self::Unresolved,
            _ => unreachable!(),
        }
    }
}

impl From<&Type> for FloatLitType {
    fn from(value: &Type) -> Self {
        match value {
            Type::F32 => Self::F32,
            Type::F64 => Self::F64,
            Type::Float => Self::Unresolved,
            _ => unreachable!(),
        }
    }
}

#[macro_export]
macro_rules! ty {
    (*void) => {
        Type::VoidPointer
    };
    (*$($tt:tt)+) => {
        Type::Pointer(Box::new(ty!($($tt)+)))
    };
    (i8) => { Type::I8 };
    (i16) => { Type::I16 };
    (i32) => { Type::I32 };
    (i64) => { Type::I64 };
    (u8) => { Type::U8 };
    (u16) => { Type::U16 };
    (u32) => { Type::U32 };
    (u64) => { Type::U64 };
    (Integer) => { Type::Integer };
    (f32) => { Type::F32 };
    (f64) => { Type::F64 };
    (Float) => { Type::Float };
    (bool) => { Type::Bool };
}

impl Type {
    pub fn from_atom(atom: &TypeAtom) -> Option<Type> {
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
                    "f64" => Some(Self::F64),

                    "fatptr" => Some(Self::FatPointer), // TODO: "fatptr" is bad
                    "bool" => Some(Self::Bool),
                    _ => None,
                }
            }
            TypeAtom::VoidPointer => Some(Self::VoidPointer),
            TypeAtom::Pointer(inner) => Some(Self::Pointer(Box::new(Self::from_atom(inner)?))),
        }
    }

    /// Return size (in bytes) of this type
    pub fn size(&self) -> u32 {
        match self {
            Type::I8 | Type::U8 | Type::Bool => 1,
            Type::I16 | Type::U16 => 2,
            Type::I32 | Type::U32 | Type::F32 => 4,
            Type::I64 | Type::U64 | Type::F64 => 8,
            Type::Integer | Type::Float => 8, // assume [if]64
            Type::Pointer(_) | Type::VoidPointer => 8, // TODO: platform pointer size
            Type::FatPointer => todo!("fat pointer size"),
            Type::Struct => todo!("structs"),
        }
    }

    /// Return size (in bytes) of this type after being padded to 8 bytes
    pub fn padded_size(&self) -> u32 {
        match self {
            Type::I8 | Type::U8 | Type::Bool => 8,
            Type::I16 | Type::U16 => 8,
            Type::I32 | Type::U32 => 8,
            Type::F32 | Type::Float => 8,
            Type::I64 | Type::U64 | Type::F64 => 8,
            Type::Integer => 8,                        // assume [ui]64
            Type::Pointer(_) | Type::VoidPointer => 8, // TODO: platform pointer size
            Type::FatPointer => todo!("fat pointer size"),
            Type::Struct => todo!("structs"),
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            Type::Pointer(_) | Type::VoidPointer => true,
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Integer
            | Type::F32
            | Type::F64
            | Type::Float
            | Type::FatPointer
            | Type::Struct
            | Type::Bool => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Integer
            | Type::Pointer(_)
            | Type::VoidPointer => true,
            Type::F32 | Type::F64 | Type::Float | Type::FatPointer | Type::Struct | Type::Bool => {
                false
            }
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::F32 | Type::F64 | Type::Float => true,
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Integer
            | Type::Pointer(_)
            | Type::VoidPointer
            | Type::FatPointer
            | Type::Struct
            | Type::Bool => false,
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 => false,
            Type::U8 | Type::U16 | Type::U32 | Type::U64 => false,
            Type::Integer => true, // assume true
            Type::F32 | Type::F64 | Type::Float => true,
            Type::Pointer(_) | Type::VoidPointer => false,
            Type::FatPointer => todo!(),
            Type::Struct => todo!(),
            Type::Bool => false,
        }
    }

    /// Type has not yet been resolved (Type::Integer or Type::Float)
    pub fn is_unresolved(&self) -> bool {
        match self {
            Type::Integer | Type::Float => true,
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::F32
            | Type::F64
            | Type::Pointer(_)
            | Type::VoidPointer
            | Type::FatPointer
            | Type::Struct
            | Type::Bool => false,
        }
    }
    /// self == other
    pub fn equal(&self, other: &Type) -> Option<Vec<Self>> {
        if self == other {
            return Some(vec![ty!(bool)]);
        }
        match self {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64 => match other {
                Type::Integer => {}
                _ => return None,
            },
            Type::Integer => match other {
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64 => {}
                _ => return None,
            },
            Type::F32 | Type::F64 => match other {
                Type::Float => {}
                _ => return None,
            },
            Type::Float => match other {
                Type::F32 | Type::F64 => {}
                _ => return None,
            },
            Type::Pointer(_) | Type::VoidPointer => return None,
            Type::FatPointer => return None,
            Type::Struct => return None,
            Type::Bool => return None,
        }
        Some(vec![ty!(bool)])
    }

    /// self {<,>,<=,>=} other
    pub fn ord_cmp(&self, other: &Type) -> Option<Vec<Self>> {
        if self == other {
            return Some(vec![ty!(bool)]);
        }
        match self {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64 => match other {
                Type::Integer => {}
                _ => return None,
            },
            Type::Integer => match other {
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64 => {}
                _ => return None,
            },
            Type::F32 | Type::F64 => match other {
                Type::Float => {}
                _ => return None,
            },
            Type::Float => match other {
                Type::F32 | Type::F64 => {}
                _ => return None,
            },
            Type::Pointer(_) | Type::VoidPointer => return None,
            Type::FatPointer => return None,
            Type::Struct => return None,
            Type::Bool => return None,
        }
        Some(vec![ty!(bool)])
    }

    /// self % other
    pub fn modulo(&self, other: &Type) -> Option<Vec<Self>> {
        if self.is_integer() && self == other || *self == Type::Integer && other.is_integer() {
            return Some(vec![self.clone()]);
        }
        match self {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Integer => match other {
                Type::Integer => Some(vec![self.clone()]),
                _ => None,
            },
            Type::F32 | Type::F64 | Type::Float => None,
            Type::Pointer(_) | Type::VoidPointer => match other {
                Type::Integer => Some(vec![self.clone()]),
                _ => None,
            },
            Type::FatPointer => None,
            Type::Struct => None,
            Type::Bool => None,
        }
    }

    /// self + other
    pub fn add(&self, other: &Type) -> Option<Vec<Self>> {
        match self {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Integer => match other {
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Integer => Some(vec![self.clone()]),
                Type::F32 => None,
                Type::F64 => None,
                Type::Float => None,
                Type::Pointer(_) | Type::VoidPointer => Some(vec![other.clone()]),
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::F32 | Type::F64 | Type::Float => match other {
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Integer
                | Type::F32
                | Type::F64
                | Type::Float => Some(vec![self.clone()]),
                Type::Pointer(_) | Type::VoidPointer => None,
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::Pointer(_) | Type::VoidPointer => match other {
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Integer => Some(vec![self.clone()]),
                Type::F32 | Type::F64 | Type::Float => None,
                Type::Pointer(_) | Type::VoidPointer => None,
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::FatPointer => None,
            Type::Struct => None,
            Type::Bool => None,
        }
    }

    /// self - other
    pub fn sub(&self, other: &Type) -> Option<Vec<Self>> {
        match self {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Integer => match other {
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Integer => Some(vec![self.clone()]),
                Type::F32 => None,
                Type::F64 => None,
                Type::Float => None,
                Type::Pointer(_) | Type::VoidPointer => Some(vec![other.clone()]),
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::F32 | Type::F64 | Type::Float => match other {
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Integer
                | Type::F32
                | Type::F64
                | Type::Float => Some(vec![self.clone()]),
                Type::Pointer(_) | Type::VoidPointer => None,
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::Pointer(_) | Type::VoidPointer => match other {
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Integer => None,
                Type::F32 | Type::F64 | Type::Float => None,
                Type::Pointer(_) | Type::VoidPointer => None,
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::FatPointer => None,
            Type::Struct => None,
            Type::Bool => None,
        }
    }

    /// self / other
    pub fn div(&self, other: &Type) -> Option<Vec<Self>> {
        match self {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Integer => match other {
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Integer => Some(vec![self.clone()]),
                Type::F32 => None,
                Type::F64 => None,
                Type::Float => None,
                Type::Pointer(_) | Type::VoidPointer => None,
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::F32 | Type::F64 | Type::Float => match other {
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Integer => None,
                Type::F32 | Type::F64 | Type::Float => Some(vec![self.clone()]),
                Type::Pointer(_) | Type::VoidPointer => None,
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::Pointer(_) | Type::VoidPointer => None,
            Type::FatPointer => None,
            Type::Struct => None,
            Type::Bool => None,
        }
    }

    /// self * other
    pub fn mul(&self, other: &Type) -> Option<Vec<Self>> {
        match self {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Integer => match other {
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Integer => Some(vec![self.clone()]),
                Type::F32 => None,
                Type::F64 => None,
                Type::Float => None,
                Type::Pointer(_) | Type::VoidPointer => None,
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::F32 | Type::F64 | Type::Float => match other {
                Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Integer
                | Type::F32
                | Type::F64
                | Type::Float => Some(vec![self.clone()]),
                Type::Pointer(_) | Type::VoidPointer => None,
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::Pointer(_) | Type::VoidPointer => None,
            Type::FatPointer => None,
            Type::Struct => None,
            Type::Bool => None,
        }
    }

    pub fn matches(&self, expected: &Type) -> bool {
        if self == expected {
            return true;
        }
        match expected {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64 => matches!(self, Type::Integer),
            Type::Integer => unreachable!(),
            Type::F32 | Type::F64 => matches!(self, Type::Float),
            Type::Float => unreachable!(),
            Type::Pointer(_) | Type::VoidPointer => false, // TODO: should we have implicit *void -> *T?
            Type::FatPointer => false,
            Type::Struct => false,
            Type::Bool => false,
        }
    }

    pub fn cast_into(&self, span: Span, target: &Self) -> Result<(), IrGenError> {
        if self == target {
            return Ok(());
        }
        let valid = match self {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Integer => {
                !target.is_unresolved()
                    && (target.is_integer() || target.is_float() || *target == Type::Bool)
            }
            Type::Float | Type::F32 | Type::F64 => {
                !target.is_unresolved()
                    && (target.is_integer() || target.is_float() || *target == Type::Bool)
            }
            Type::Pointer(_) | Type::VoidPointer => matches!(target, Type::U64 | Type::Pointer(_)), // pointer case not caught if inner type not equal
            Type::FatPointer => todo!(),
            Type::Struct => todo!(),
            Type::Bool => !target.is_unresolved() && target.is_integer(),
        };
        if valid {
            Ok(())
        } else {
            Err(IrGenError::TypeError2 {
                span,
                message: format!("Cannot cast {} into {}", self, target),
            })
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::Integer => write!(f, "{{integer}}"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::Float => write!(f, "{{float}}"),
            Type::Pointer(inner) => write!(f, "*{}", inner),
            Type::VoidPointer => write!(f, "*void"),
            Type::FatPointer => write!(f, "fatptr"),
            Type::Struct => write!(f, "fatptr"),
            Type::Bool => write!(f, "bool"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    Not,
    NotEqual,
    Lt,
    Lte,
    Gt,
    Gte,
    Dup(u32),
    Swap,
    Drop,
    Load,
    Store,
}

impl Builtin {
    pub fn type_check(
        self,
        span: Span,
        type_stack: &[Type],
    ) -> Result<(usize, Vec<Type>), IrGenError> {
        match self {
            Builtin::Add => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.add(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot add {} to {}", a, b),
                })?;
                Ok((2, returns))
            }
            Builtin::Sub => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.sub(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot subtract {} from {}", a, b),
                })?;
                Ok((2, returns))
            }
            Builtin::Mul => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.mul(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot multiply {} by {}", a, b),
                })?;
                Ok((2, returns))
            }
            Builtin::Div => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.div(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot multiply {} by {}", b, a),
                })?;
                Ok((2, returns))
            }
            Builtin::Mod => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.modulo(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot perform module of {} by {}", a, b),
                })?;
                Ok((2, returns))
            }
            Builtin::Equal => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.equal(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot compare {} to {}", b, a),
                })?;
                Ok((2, returns))
            }
            Builtin::Not => {
                let a = type_stack
                    .last()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                if *a == Type::Bool {
                    Ok((1, vec![a.clone()]))
                } else {
                    Err(IrGenError::TypeError2 {
                        span,
                        message: format!("Cannot negate {}", a),
                    })
                }
            }
            Builtin::NotEqual => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.equal(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot compare {} to {}", b, a),
                })?;
                Ok((2, returns))
            }
            Builtin::Lt => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.ord_cmp(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot compare {} and {}", a, b),
                })?;
                Ok((2, returns))
            }
            Builtin::Lte => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.ord_cmp(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot compare {} and {}", a, b),
                })?;
                Ok((2, returns))
            }
            Builtin::Gt => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.ord_cmp(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot compare {} and {}", a, b),
                })?;
                Ok((2, returns))
            }
            Builtin::Gte => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.ord_cmp(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot compare {} and {}", a, b),
                })?;
                Ok((2, returns))
            }
            Builtin::Dup(n) => {
                if type_stack.len() < n as _ {
                    return Err(IrGenError::StackUnderflow { span });
                }
                let items = &type_stack[type_stack.len() - n as usize..];
                let items: Vec<_> = items.iter().chain(items).cloned().collect();
                Ok((n as _, items))
            }
            Builtin::Swap => {
                let [a, b] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                Ok((2, vec![b.clone(), a.clone()]))
            }
            Builtin::Drop => {
                type_stack
                    .last()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                Ok((1, Vec::new()))
            }
            Builtin::Load => {
                let x = type_stack
                    .last()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                // TODO: load from cstring
                let Type::Pointer(ty) = x else {
                    return Err(IrGenError::TypeError2 {
                        span,
                        message: format!("Cannot dereference {}", x),
                    });
                };
                Ok((1, vec![ty.as_ref().clone()]))
            }
            Builtin::Store => {
                let [pointer, value] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let Type::Pointer(inner) = pointer else {
                    return Err(IrGenError::TypeError2 {
                        span,
                        message: format!("Cannot store {} into {}", value, pointer),
                    });
                };
                if **inner != *value {
                    return Err(IrGenError::TypeError2 {
                        span,
                        message: format!("Cannot store {} into {}", value, pointer),
                    });
                }
                Ok((2, Vec::new()))
            }
        }
    }

    pub fn apply(
        self,
        current: Ir,
        ir_stack: &mut Vec<Ir>,
        type_stack: &[Type],
    ) -> Result<(), IrGenError> {
        match self {
            Builtin::Add => {
                let [a_ty, b_ty] = type_stack else {
                    unreachable!("checked by caller")
                };
                let b_ir = ir_stack.pop().expect("checked by caller");
                let a_ir = ir_stack.pop().expect("checked by caller");
                let result_ty = a_ty.add(b_ty).unwrap().pop().unwrap();

                let span = b_ir.span + a_ir.span + current.span;
                match b_ty {
                    Type::I8
                    | Type::I16
                    | Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::Integer => {
                        let (IrKind::PushInt(b, _), IrKind::PushInt(a, _)) =
                            (&b_ir.kind, &a_ir.kind)
                        else {
                            panic!("checked by caller")
                        };
                        ir_stack.push(Ir::new(span, IrKind::PushInt(*a + *b, result_ty.into())))
                    }
                    Type::F32 | Type::F64 | Type::Float => {
                        let (IrKind::PushFloat(b, _), IrKind::PushFloat(a, _)) =
                            (&b_ir.kind, &a_ir.kind)
                        else {
                            panic!("checked by caller")
                        };
                        ir_stack.push(Ir::new(span, IrKind::PushFloat(*a + *b, result_ty.into())))
                    }
                    Type::Pointer(_) | Type::VoidPointer => todo!(),
                    Type::FatPointer => todo!(),
                    Type::Struct => todo!(),
                    Type::Bool => todo!(),
                };

                Ok(())
            }
            Builtin::Sub => todo!(),
            Builtin::Mul => {
                let [a_ty, b_ty] = type_stack else {
                    unreachable!("checked by caller")
                };
                let b_ir = ir_stack.pop().expect("checked by caller");
                let a_ir = ir_stack.pop().expect("checked by caller");
                let result_ty = a_ty.mul(b_ty).unwrap().pop().unwrap();

                let span = b_ir.span + a_ir.span + current.span;
                match b_ty {
                    Type::I8
                    | Type::I16
                    | Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::Integer => {
                        let (IrKind::PushInt(b, _), IrKind::PushInt(a, _)) =
                            (&b_ir.kind, &a_ir.kind)
                        else {
                            panic!("checked by caller")
                        };
                        ir_stack.push(Ir::new(span, IrKind::PushInt(*a * *b, result_ty.into())))
                    }
                    Type::F32 | Type::F64 | Type::Float => {
                        let (IrKind::PushFloat(b, _), IrKind::PushFloat(a, _)) =
                            (&b_ir.kind, &a_ir.kind)
                        else {
                            panic!("checked by caller")
                        };
                        ir_stack.push(Ir::new(span, IrKind::PushFloat(*a * *b, result_ty.into())))
                    }
                    Type::Pointer(_) | Type::VoidPointer => todo!(),
                    Type::FatPointer => todo!(),
                    Type::Struct => todo!(),
                    Type::Bool => todo!(),
                };

                Ok(())
            }
            Builtin::Div => todo!(),
            Builtin::Mod => todo!(),
            Builtin::Equal => {
                let [a_ty, b_ty] = type_stack else {
                    unreachable!("checked by caller")
                };
                let b_ir = ir_stack.pop().expect("checked by caller");
                let a_ir = ir_stack.pop().expect("checked by caller");
                assert!(a_ty.equal(b_ty).is_some());

                let span = b_ir.span + a_ir.span + current.span;
                match b_ty {
                    Type::I8
                    | Type::I16
                    | Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::Integer => {
                        let (IrKind::PushInt(b, _), IrKind::PushInt(a, _)) =
                            (&b_ir.kind, &a_ir.kind)
                        else {
                            panic!("checked by caller")
                        };
                        ir_stack.push(Ir::new(span, IrKind::PushBool(*a == *b)))
                    }
                    Type::F32 | Type::F64 | Type::Float => {
                        let (IrKind::PushFloat(b, _), IrKind::PushFloat(a, _)) =
                            (&b_ir.kind, &a_ir.kind)
                        else {
                            panic!("checked by caller")
                        };
                        ir_stack.push(Ir::new(span, IrKind::PushBool(*a == *b)))
                    }
                    Type::Pointer(_) | Type::VoidPointer => todo!(),
                    Type::FatPointer => todo!(),
                    Type::Struct => todo!(),
                    Type::Bool => {
                        let (IrKind::PushBool(b), IrKind::PushBool(a)) = (&b_ir.kind, &a_ir.kind)
                        else {
                            panic!("checked by caller")
                        };
                        ir_stack.push(Ir::new(span, IrKind::PushBool(*a == *b)))
                    }
                };
                Ok(())
            }
            Builtin::Not => {
                let b_ir = ir_stack.pop().unwrap();
                let IrKind::PushBool(b) = b_ir.kind else {
                    panic!("checked by caller")
                };
                ir_stack.push(Ir::new(current.span + b_ir.span, IrKind::PushBool(!b)));
                Ok(())
            }
            Builtin::NotEqual => todo!(),
            Builtin::Lt => todo!(),
            Builtin::Lte => todo!(),
            Builtin::Gt => todo!(),
            Builtin::Gte => todo!(),
            Builtin::Dup(n) => {
                ir_stack.extend_from_within(ir_stack.len() - n as usize..);
                Ok(())
            }
            Builtin::Swap => {
                let a_ir = ir_stack.pop().expect("checked by caller");
                let b_ir = ir_stack.pop().expect("checked by caller");
                ir_stack.push(a_ir);
                ir_stack.push(b_ir);
                Ok(())
            }
            Builtin::Drop => {
                ir_stack.pop().expect("checked by caller");
                Ok(())
            }
            // TODO: load from cstring
            Builtin::Load => Ok(()),
            Builtin::Store => Ok(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct WhileIr {
    pub loop_id: String,
    pub condition: Vec<Ir>,
    pub condition_span: Span,
    pub body: Vec<Ir>,
    pub body_span: Span,
}

#[derive(Debug, Clone)]
pub struct ElseThenIr {
    pub then_span: Span,
    pub condition: Vec<Ir>,
    pub body: Vec<Ir>,
}

#[derive(Debug, Clone)]
pub struct ThenIr {
    pub then_span: Span,
    pub body: Vec<Ir>,
    pub else_thens: Vec<ElseThenIr>,
    pub elze: Vec<Ir>,
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub target: Type,
    pub span: Span,
}

impl Cast {
    pub fn apply(&self, ir: Ir, ir_stack: &mut Vec<Ir>) -> Result<(), IrGenError> {
        match ir.kind {
            IrKind::PushInt(n, _) => {
                if self.target.is_integer() {
                    let n = n & ((1 << (8 * self.target.size())) - 1);
                    ir_stack.push(Ir::new(ir.span, IrKind::PushInt(n, (&self.target).into())));
                } else if self.target == Type::Bool {
                    ir_stack.push(Ir::new(ir.span, IrKind::PushBool(n != 0)));
                } else if self.target.is_float() {
                    let f = n as f64;
                    ir_stack.push(Ir::new(
                        ir.span,
                        IrKind::PushFloat(f, (&self.target).into()),
                    ));
                } else {
                    panic!()
                }
            }
            IrKind::PushFloat(f, _) => {
                if self.target.is_integer() {
                    let n = f as i128 & ((1 << (8 * self.target.size())) - 1);
                    ir_stack.push(Ir::new(ir.span, IrKind::PushInt(n, (&self.target).into())));
                } else if self.target.is_float() {
                    ir_stack.push(Ir::new(
                        ir.span,
                        IrKind::PushFloat(f, (&self.target).into()),
                    ));
                } else {
                    panic!()
                }
            }
            IrKind::PushBool(b) => {
                let kind = match self.target {
                    Type::I8
                    | Type::I16
                    | Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::Integer => {
                        IrKind::PushInt(if b { 1 } else { 0 }, IntLitType::Unresolved)
                    }
                    Type::F32 => todo!(),
                    Type::F64 => todo!(),
                    Type::Float => todo!(),
                    Type::Pointer(_) | Type::VoidPointer => todo!(),
                    Type::FatPointer => todo!(),
                    Type::Struct => todo!(),
                    Type::Bool => IrKind::PushBool(b),
                };
                ir_stack.push(Ir::new(ir.span, kind));
            }
            IrKind::PushCStr(_) => todo!(),
            IrKind::PushStr(_) => todo!(),
            _ => panic!("checked by compiler"),
        }

        Ok(())
    }

    pub fn type_check(&self, type_stack: &mut TypeStack) -> Result<(), IrGenError> {
        let ty = type_stack.pop(self.span)?;
        ty.cast_into(self.span, &self.target)?;
        type_stack.push(self.target.clone());
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum IrKind {
    /// Integer literal, to be coerced / operated on by const fns
    PushInt(i128, IntLitType),
    /// Float literal, to be coerced / operated on by const fns
    PushFloat(f64, FloatLitType),
    /// Boolean literal to be operated on by const fns
    PushBool(bool),
    /// C String literal, becomes `*i8`
    PushCStr(CString),
    /// String literal, becomes fat pointer (tbd)
    PushStr(String),
    /// Call a named function.  If the function is constant and arguments are constant, the
    /// function will be applied in optimise.
    CallFn(Ident),
    /// Call a builtin function.  If the arguments are constant, the function will be applied in
    /// optimise
    CallBuiltin(Builtin),
    // TODO: how to const
    Then(ThenIr),
    While(WhileIr),
    // string is loop id
    Break(String),
    Cast(Cast),
}

impl IrKind {
    pub const fn is_const(&self) -> bool {
        match self {
            IrKind::PushInt(_, _) | IrKind::PushFloat(_, _) | IrKind::PushBool(_) => true,
            IrKind::PushCStr(_)
            | IrKind::PushStr(_)
            | IrKind::CallFn(_)
            | IrKind::CallBuiltin(_)
            | IrKind::Then(_)
            | IrKind::While(_)
            | IrKind::Break(_)
            | IrKind::Cast(_) => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ir {
    pub span: Span,
    pub kind: IrKind,
}

impl Ir {
    fn new(span: Span, kind: IrKind) -> Self {
        Self { span, kind }
    }
}

/// Function that has been converted into IR
#[derive(Debug)]
pub struct ConvertedFunction {
    pub name: String,
    pub linker_name: String,
    pub body: Vec<Ir>,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub linker_name: String,
    pub ident_span: Span,
    pub visibility: Visibility,
    pub args: TypeStack,
    pub variadic: bool,
    pub returns: TypeStack,
    pub external: bool,
}

impl FunctionSignature {
    pub fn apply(&self, ident: &Ident, type_stack: &mut TypeStack) -> Result<(), IrGenError> {
        if let Some(len) = ident.arity {
            if self.variadic && (len as usize) < self.args.len() {
                return Err(IrGenError::IncorrectExplicitVariadicArgSize {
                    expected: self.args.len(),
                    actual: len as usize,
                    span: ident.span(),
                });
            } else if !self.variadic && self.args.len() != len as usize {
                return Err(IrGenError::IncorrectExplicitArgSize {
                    expected: self.args.len(),
                    actual: len as usize,
                    span: ident.span(),
                });
            }
        }
        for expected in self.args.iter().rev() {
            // TODO: error for all args at the same time
            type_stack.pop_type(ident.span(), expected)?;
        }
        if let Some(len) = ident.arity
            && len as usize > self.args.len()
            && self.variadic
        {
            for _ in 0..(len as usize - self.args.len()) {
                type_stack.pop(ident.span())?;
            }
        }
        type_stack.extend_from_slice(&self.returns);
        Ok(())
    }
}

impl Display for FunctionSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.external {
            if self.linker_name != self.name {
                write!(f, "extern(\"{}\") ", self.linker_name)?;
            } else {
                write!(f, "extern ")?;
            }
        }
        write!(f, "fn {}", self.name)?;
        if !self.args.is_empty() || self.variadic {
            write!(f, "(")?;
            for (i, a) in self.args.iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", a)?;
            }
            if self.variadic {
                if !self.args.is_empty() {
                    write!(f, " ")?;
                }
                write!(f, "...")?;
            }
            write!(f, ")")?;
        }

        if !self.returns.is_empty() {
            write!(f, " -> (")?;
            for (i, r) in self.returns.iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", r)?;
            }
            write!(f, ")")?;
        }
        write!(f, ";")?;

        Ok(())
    }
}

struct DisplayAsDebug<T>(T);
impl<T: Display> Debug for DisplayAsDebug<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <T as Display>::fmt(&self.0, f)
    }
}

#[derive(Clone)]
pub struct TypeStack {
    inner: Vec<Type>,
}

impl std::fmt::Debug for TypeStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.inner.iter().map(DisplayAsDebug))
            .finish()
    }
}

impl TypeStack {
    pub fn new() -> Self {
        Self { inner: Vec::new() }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            inner: Vec::with_capacity(cap),
        }
    }

    pub fn pop(&mut self, span: Span) -> Result<Type, IrGenError> {
        self.inner.pop().ok_or(IrGenError::StackUnderflow { span })
    }

    pub fn pop_type(&mut self, span: Span, expected: &Type) -> Result<(), IrGenError> {
        let last = self.pop(span)?;
        if last.matches(expected) {
            Ok(())
        } else {
            Err(IrGenError::TypeError2 {
                span,
                message: format!(
                    "Type error: expected '{}' on stack, found '{}'",
                    expected, last
                ),
            })
        }
    }

    pub fn matches(&self, expected: &TypeStack) -> bool {
        self.len() == expected.len()
            && self
                .iter()
                .zip(expected.iter())
                .all(|(actual, expected)| actual.matches(expected))
    }
}

impl Default for TypeStack {
    fn default() -> Self {
        Self::new()
    }
}

impl From<Vec<Type>> for TypeStack {
    fn from(value: Vec<Type>) -> Self {
        Self { inner: value }
    }
}

impl Deref for TypeStack {
    type Target = Vec<Type>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for TypeStack {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub enum ResolvedIdent<'a> {
    Function(&'a FunctionSignature),
    Module(&'a Module),
}

impl<'a> ResolvedIdent<'a> {
    pub fn unwrap_function(self) -> &'a FunctionSignature {
        match self {
            ResolvedIdent::Function(f) => f,
            ResolvedIdent::Module(_) => panic!("Expected function, found module"),
        }
    }
}

#[derive(Debug, Default)]
pub struct Module {
    /// The ASTs for the module
    asts: Vec<Ast>,

    quiet: bool,
    /// None until `update_root` called
    root: Option<Rc<Module>>,

    pub path: Vec<String>,
    pub filepath: PathBuf,
    pub content: String,
    pub submodules: HashMap<String, (Module, Visibility)>,
    pub imports: HashMap<String, Path>,
    pub functions: HashMap<String, FunctionSignature>,
    pub converted_functions: Vec<ConvertedFunction>,
}

impl Module {
    pub fn new(
        asts: Vec<Ast>,
        path: Vec<String>,
        filepath: PathBuf,
        content: String,
        quiet: bool,
    ) -> Self {
        Self {
            asts,
            quiet,
            root: None,
            path,
            filepath,
            content,
            submodules: Default::default(),
            imports: Default::default(),
            functions: Default::default(),
            converted_functions: Default::default(),
        }
    }

    /// Recursively clone this module, only cloning information about the module, not the file
    /// content or compiled content.
    pub fn light_clone(&self) -> Self {
        Self {
            asts: self.asts.clone(),
            quiet: self.quiet,
            root: self.root.as_ref().map(Rc::clone),
            path: self.path.clone(),
            filepath: self.filepath.clone(),
            content: Default::default(),
            submodules: self
                .submodules
                .iter()
                .map(|(n, (m, v))| (n.clone(), (m.light_clone(), *v)))
                .collect(),
            imports: self.imports.clone(),
            functions: self.functions.clone(),
            converted_functions: Vec::new(),
        }
    }

    fn resolve_ident_rec(
        &self,
        path: &[PathElement],
        depth: usize,
    ) -> Result<(ResolvedIdent<'_>, &Module), IrGenError> {
        match path {
            [] => unreachable!("paths can not be empty"),
            [last] => {
                let f = if let Some(i) = self.imports.get(&last.name) {
                    return self.resolve_ident(i);
                } else if let Some(f) = self.functions.get(&last.name) {
                    ResolvedIdent::Function(f)
                } else if let Some((m, vis)) = self.submodules.get(&last.name) {
                    if depth > 1 && *vis == Visibility::Private {
                        return Err(IrGenError::PrivateModule {
                            name: m.path.last().unwrap().clone(),
                            span: last.span(),
                        });
                    }
                    ResolvedIdent::Module(m)
                } else {
                    return Err(IrGenError::UndefinedSymbol {
                        ident: last.name.clone(),
                        span: last.span(),
                    });
                };
                Ok((f, self))
            }
            [next, rest @ ..] => {
                let (submodule, visibility) = self.submodules.get(&next.name).ok_or_else(|| {
                    IrGenError::UnknownSubmodule {
                        name: next.name.clone(),
                        span: next.span(),
                    }
                })?;

                if depth > 1 && *visibility == Visibility::Private {
                    return Err(IrGenError::PrivateModule {
                        name: submodule.path.last().unwrap().clone(),
                        span: next.span(),
                    });
                }

                submodule.resolve_ident_rec(rest, depth + 1)
            }
        }
    }

    pub fn resolve_ident(&self, path: &Path) -> Result<(ResolvedIdent<'_>, &Module), IrGenError> {
        let module = if path.is_root {
            self.root.as_deref().unwrap_or(self)
        } else {
            self
        };
        let (f, module) = module.resolve_ident_rec(path, 1)?;
        if path.len() > 1
            && let ResolvedIdent::Function(f) = f
            && f.visibility != Visibility::Public
        {
            return Err(IrGenError::PrivateFunction {
                name: f.name.clone(),
                span: path.span(),
            });
        }
        Ok((f, module))
    }

    pub fn resolve_path(&self, path: &[String]) -> Option<(&FunctionSignature, &Module)> {
        // TODO: This may resolve to more than just functions
        match path {
            [] => unreachable!("paths can not be empty"),
            [last] => self.functions.get(last).map(|f| (f, self)),
            [next, rest @ ..] => self.submodules.get(next)?.0.resolve_path(rest),
        }
    }

    pub fn check_imports(&self) -> Result<(), IrGenError> {
        for (_, path) in self.imports.iter() {
            self.resolve_ident(path)?;
        }
        Ok(())
    }

    pub fn compile_module(&mut self) -> Result<(), IrGenError> {
        self.check_imports()?;
        for ast in std::mem::take(&mut self.asts) {
            self.module_ast(ast)?;
        }
        Ok(())
    }

    pub fn parse_module(&self, module: &ModuleDeclaration) -> Result<Module, IrGenError> {
        // TODO: handle cyclic dependency
        let paths = [
            // foo.st
            self.filepath
                .with_file_name(&module.name)
                .with_extension("st"),
            // foo/mod.st
            self.filepath.with_file_name(&module.name).join("mod.st"),
        ];
        let path =
            paths
                .iter()
                .find(|p| p.exists())
                .ok_or_else(|| IrGenError::MissingModuleFile {
                    name: module.name.clone(),
                    span: module.name_span,
                    checked_paths: paths.to_vec(),
                })?;

        let content = std::fs::read_to_string(path).map_err(|e| IrGenError::ModuleReadError {
            source: e,
            span: module.span(),
            path: path.clone(),
        })?;
        let lex = Lexer::new(&content, path.to_str().unwrap());
        let parser = Parser::new(lex);
        let ast = wrap_miette(parser.parse_module(), path, &content)
            .map_err(IrGenError::ModuleParseError)?;
        let mut new_path = self.path.clone();
        new_path.push(module.name.clone());
        let mut module = Module::new(ast, new_path, path.clone(), content, self.quiet);
        wrap_miette(module.scan_functions(), path, &module.content)
            .map_err(IrGenError::ModuleIrGenError)?;
        Ok(module)
    }

    pub fn scan_functions(&mut self) -> Result<(), IrGenError> {
        for ast in &self.asts {
            match ast {
                Ast::Ident(_)
                | Ast::Atom(_)
                | Ast::Then(_)
                | Ast::While(_)
                | Ast::Cast(_)
                | Ast::Dup(_) => continue,
                Ast::ExternFn(f) => {
                    if let Some(original) = self.functions.get(&f.name) {
                        return Err(IrGenError::RepeatDefinition {
                            ident: f.name.clone(),
                            original: original.ident_span,
                            repeat: f.ident_span,
                        });
                    }
                    self.functions.insert(
                        f.name.clone(),
                        FunctionSignature {
                            name: f.name.clone(),
                            linker_name: f.linker_name.clone(),
                            ident_span: f.ident_span,
                            visibility: f.visibility,
                            args: f
                                .args
                                .iter()
                                .rev()
                                .map(|t| Type::from_atom(t).expect("TODO"))
                                .collect::<Vec<_>>()
                                .into(),
                            variadic: f.variadic,
                            returns: f
                                .returns
                                .iter()
                                .rev()
                                .map(|t| Type::from_atom(t).expect("TODO"))
                                .collect::<Vec<_>>()
                                .into(),
                            external: true,
                        },
                    );
                }
                Ast::Fn(f) => {
                    if let Some(original) = self.functions.get(&f.name) {
                        return Err(IrGenError::RepeatDefinition {
                            ident: f.name.clone(),
                            original: original.ident_span,
                            repeat: f.ident_span,
                        });
                    }
                    self.functions.insert(
                        f.name.clone(),
                        FunctionSignature {
                            name: f.name.clone(),
                            linker_name: f.name.clone(), // TODO: support custom linker name
                            ident_span: f.ident_span,
                            visibility: f.visibility,
                            args: f
                                .args
                                .iter()
                                .rev()
                                .map(|t| Type::from_atom(t).expect("TODO"))
                                .collect::<Vec<_>>()
                                .into(),
                            variadic: false, // user-defined functions can not be variadic
                            returns: f
                                .returns
                                .iter()
                                .rev()
                                .map(|t| Type::from_atom(t).expect("TODO"))
                                .collect::<Vec<_>>()
                                .into(),
                            external: false,
                        },
                    );
                }
                Ast::ModuleDeclaration(module) => {
                    // TODO: better file resolution
                    let parsed = self.parse_module(module)?;
                    self.submodules
                        .insert(module.name.clone(), (parsed, module.visibility));
                }
                Ast::Import(import) => {
                    if import.visibility != Visibility::Private {
                        todo!("Non-private import visibility");
                    }
                    Self::add_imports(&mut self.imports, &import.tree, &mut vec![], import.is_root);
                }
            }
        }
        Ok(())
    }

    pub fn update_roots(&mut self, root: Rc<Module>) {
        self.root = Some(Rc::clone(&root));
        for (m, _) in self.submodules.values_mut() {
            m.update_roots(Rc::clone(&root));
        }
    }

    fn add_imports(
        imports: &mut HashMap<String, Path>,
        tree: &ImportTree,
        current: &mut Vec<PathElement>,
        is_root: bool,
    ) {
        match &tree.value {
            ImportTreeValue::Vertex { children } => {
                assert!(!children.is_empty());
                current.push(tree.name.clone());
                for child in children {
                    Self::add_imports(imports, child, current, is_root);
                }
                current.pop();
            }
            ImportTreeValue::Leaf { rename } => {
                // push before to prevent the re-allocation of pushing after clone
                current.push(tree.name.clone());
                let path = current.clone();
                current.pop();

                imports.insert(
                    rename.clone().unwrap_or_else(|| tree.name.name.clone()),
                    Path::from_vec(path, is_root),
                );
            }
        }
    }

    fn compile_body(
        &mut self,
        type_stack: &mut TypeStack,
        out: &mut Vec<Ir>,
        outer_loop: Option<&(String, TypeStack, Span)>,
        body: Vec<Ast>,
    ) -> Result<(), IrGenError> {
        for ast in body {
            self.body_ast(type_stack, out, outer_loop, ast)?;
        }
        Ok(())
    }

    fn body_ast(
        &mut self,
        type_stack: &mut TypeStack,
        out: &mut Vec<Ir>,
        outer_loop: Option<&(String, TypeStack, Span)>,
        ast: Ast,
    ) -> Result<(), IrGenError> {
        macro_rules! simple {
            ($span: expr, $type: expr, $kind: expr) => {{
                type_stack.push($type);
                out.push(Ir::new($span, $kind));
            }};
        }
        macro_rules! builtin {
            ($atom: expr, $builtin: ident) => {{
                let (consumed, returns) =
                    Builtin::$builtin.type_check($atom.token.span(), type_stack)?;
                let len = type_stack.len();
                type_stack.truncate(len - consumed);
                type_stack.extend_from_slice(&returns);
                out.push(Ir::new(
                    $atom.token.span(),
                    IrKind::CallBuiltin(Builtin::$builtin),
                ));
            }};
        }
        if !self.quiet {
            eprintln!("type_stack = {:?}", type_stack);
        }
        match ast {
            Ast::Ident(ident) => {
                let (f, _) = self.resolve_ident(&ident.path)?;
                match f {
                    ResolvedIdent::Function(f) => f.apply(&ident, type_stack)?,
                    ResolvedIdent::Module(_) => {
                        return Err(IrGenError::UnexpectedModule {
                            span: ident.path.span(),
                        })
                    }
                }
                out.push(Ir::new(ident.span(), IrKind::CallFn(ident)));
            }
            Ast::Atom(atom) => match atom.kind {
                AtomKind::NumLit(num_lit) => match num_lit.value {
                    NumLitVal::Integer(n) => {
                        simple!(
                            atom.token.span(),
                            ty!(Integer),
                            IrKind::PushInt(n, IntLitType::Unresolved)
                        )
                    }
                    NumLitVal::Float(f) => {
                        simple!(
                            atom.token.span(),
                            ty!(Float),
                            IrKind::PushFloat(f, FloatLitType::Unresolved)
                        )
                    }
                },
                AtomKind::BoolLit(b) => simple!(atom.token.span(), ty!(bool), IrKind::PushBool(b)),
                AtomKind::StrLit(s) => {
                    simple!(atom.token.span(), Type::FatPointer, IrKind::PushStr(s));
                }
                AtomKind::CStrLit(s) => simple!(atom.token.span(), ty!(*i8), IrKind::PushCStr(s)),
                AtomKind::Type(_) => todo!(),
                AtomKind::Plus => builtin!(atom, Add),
                AtomKind::Minus => builtin!(atom, Sub),
                AtomKind::Asterisk => builtin!(atom, Mul),
                AtomKind::Slash => builtin!(atom, Div),
                AtomKind::Percent => builtin!(atom, Mod),
                AtomKind::Equal => builtin!(atom, Equal),
                AtomKind::Not => builtin!(atom, Not),
                AtomKind::Neq => builtin!(atom, NotEqual),
                AtomKind::Lt => builtin!(atom, Lt),
                AtomKind::Lte => builtin!(atom, Lte),
                AtomKind::Gt => builtin!(atom, Gt),
                AtomKind::Gte => builtin!(atom, Gte),
                AtomKind::Swap => builtin!(atom, Swap),
                AtomKind::Drop => builtin!(atom, Drop),
                AtomKind::Load => builtin!(atom, Load),
                AtomKind::Store => builtin!(atom, Store),
                AtomKind::Break => {
                    if let Some((loop_id, start_ts, body_span)) = outer_loop {
                        if !type_stack.matches(start_ts) {
                            return Err(IrGenError::StackChanged {
                                span: *body_span,
                                before: start_ts.clone(),
                                after: type_stack.clone(),
                            });
                        }
                        out.push(Ir::new(atom.token.span(), IrKind::Break(loop_id.clone())));
                    } else {
                        todo!("Break not in a loop")
                    }
                }
            },
            Ast::ExternFn(_) => {}
            Ast::Fn(_) => unreachable!(),
            Ast::Then(t) => {
                type_stack.pop_type(t.then_token.span(), &Type::Bool)?;

                let before = type_stack.clone();
                let mut body = Vec::with_capacity(t.body.len());
                self.compile_body(type_stack, &mut body, outer_loop, t.body)?;

                // `.. then { }` should not add anything to the stack
                if t.elze.is_none() && !before.matches(type_stack) {
                    return Err(IrGenError::StackChanged {
                        before,
                        after: type_stack.clone(),
                        span: t.body_span,
                    });
                }

                let mut after_then = before.clone();
                std::mem::swap(type_stack, &mut after_then);

                let mut else_thens = Vec::with_capacity(t.else_thens.len());
                for et in t.else_thens {
                    // reset type stack
                    type_stack.clear();
                    type_stack.extend_from_slice(&before);
                    let mut condition = Vec::with_capacity(et.condition.len());
                    let then_span = et.then_token.span();
                    self.compile_body(type_stack, &mut condition, outer_loop, et.condition)?;
                    type_stack.pop_type(et.then_token.span(), &Type::Bool)?;

                    let mut body = Vec::with_capacity(et.body.len());
                    self.compile_body(type_stack, &mut body, outer_loop, et.body)?;
                    if !type_stack.matches(&after_then) {
                        return Err(IrGenError::StackChanged {
                            before: after_then,
                            after: type_stack.clone(),
                            span: et.then_token.span(),
                        });
                    }

                    else_thens.push(ElseThenIr {
                        then_span,
                        condition,
                        body,
                    });
                }

                // reset type stack
                type_stack.clear();
                type_stack.extend_from_slice(&before);
                let elze = if let Some((elze_body, token)) = t.elze {
                    let mut body = Vec::with_capacity(elze_body.len());
                    self.compile_body(type_stack, &mut body, outer_loop, elze_body)?;
                    if !type_stack.matches(&after_then) {
                        return Err(IrGenError::StackChanged {
                            before: after_then,
                            after: type_stack.clone(),
                            span: token.span(),
                        });
                    }
                    body
                } else {
                    Vec::new()
                };
                type_stack.clear();
                type_stack.extend_from_slice(&after_then);

                out.push(Ir::new(
                    t.then_token.span(),
                    IrKind::Then(ThenIr {
                        then_span: t.then_token.span(),
                        body,
                        else_thens,
                        elze,
                    }),
                ));
            }
            Ast::While(w) => {
                let before = type_stack.clone();
                let mut condition = Vec::with_capacity(w.condition.len());
                let span = w.span();
                let loop_id = format!("while_{}", span.start());
                self.compile_body(type_stack, &mut condition, None, w.condition)?;

                // bool should have been added to the stack
                type_stack.pop_type(w.while_token.span(), &Type::Bool)?;

                // After bool from condition is popped, we should have the original stack
                if !before.matches(type_stack) {
                    return Err(IrGenError::StackChanged {
                        before,
                        after: type_stack.clone(),
                        span: w.condition_span,
                    });
                }

                // compile the body
                let mut body = Vec::with_capacity(w.body.len());
                self.compile_body(
                    type_stack,
                    &mut body,
                    Some(&(loop_id.clone(), before.clone(), w.body_span)),
                    w.body,
                )?;

                // After body runs, we should have the original stack
                if !before.matches(type_stack) {
                    return Err(IrGenError::StackChanged {
                        before,
                        after: type_stack.clone(),
                        span: w.condition_span,
                    });
                }

                type_stack.clear();
                type_stack.extend_from_slice(&before);

                out.push(Ir::new(
                    span,
                    IrKind::While(WhileIr {
                        loop_id,
                        condition,
                        condition_span: w.condition_span,
                        body,
                        body_span: w.body_span,
                    }),
                ));
            }
            Ast::Cast(c) => {
                let x = Cast {
                    target: Type::from_atom(&c.target).expect("TODO: type errors"),
                    span: c.span(),
                };
                x.type_check(type_stack)?;
                out.push(Ir::new(c.span(), IrKind::Cast(x)));
            }
            Ast::Dup(d) => {
                let (consumed, returns) = Builtin::Dup(d.count).type_check(d.span(), type_stack)?;
                let len = type_stack.len();
                type_stack.truncate(len - consumed);
                type_stack.extend_from_slice(&returns);
                out.push(Ir::new(
                    d.span(),
                    IrKind::CallBuiltin(Builtin::Dup(d.count)),
                ));
            }
            Ast::ModuleDeclaration(_) => unreachable!(),
            Ast::Import(_) => unreachable!(),
        }
        Ok(())
    }

    fn module_ast(&mut self, ast: Ast) -> Result<(), IrGenError> {
        match ast {
            Ast::Ident(_) => todo!("unexpected ident"),
            Ast::Atom(_) => todo!("unexpected atom"),
            Ast::ExternFn(_) => {}
            Ast::Fn(f) => {
                let signature = self.functions[&f.name].clone();
                let mut ir = Vec::with_capacity(f.body.len());

                let mut body_type_stack = signature.args.clone();
                self.compile_body(&mut body_type_stack, &mut ir, None, f.body)?;

                let mut optimised_type_stack = signature.args.clone();
                let ir = self.optimise(ir, &mut optimised_type_stack)?;
                assert!(optimised_type_stack.matches(&body_type_stack));

                let converted = ConvertedFunction {
                    name: f.name.clone(),
                    linker_name: f.name, // TODO: user-defined linker symbols
                    body: ir,
                };
                self.converted_functions.push(converted);
                if !body_type_stack.matches(&signature.returns) {
                    return Err(IrGenError::IncorrectStackResults {
                        expected: signature.returns,
                        actual: body_type_stack,
                        span: f.ident_span,
                    });
                }
            }
            Ast::ModuleDeclaration(module) => {
                let (module, _visibility) = self
                    .submodules
                    .get_mut(&module.name)
                    .expect("we've added all modules");
                wrap_miette(module.compile_module(), &module.filepath, &module.content)
                    .map_err(IrGenError::ModuleIrGenError)?;
            }
            Ast::Import(_) => {}
            Ast::Then(_) => todo!(),
            Ast::While(_) => todo!(),
            Ast::Cast(_) => todo!(),
            Ast::Dup(_) => todo!(),
        }
        Ok(())
    }

    fn optimise(&self, ir: Vec<Ir>, type_stack: &mut TypeStack) -> Result<Vec<Ir>, IrGenError> {
        let mut out = Vec::with_capacity(ir.len());

        for ir in ir {
            update_stacks(self, ir, &mut out, type_stack)?;
        }

        Ok(out)
    }
}

pub fn update_stacks(
    module: &Module,
    mut ir: Ir,
    ir_stack: &mut Vec<Ir>,
    type_stack: &mut TypeStack,
) -> Result<(), IrGenError> {
    match ir.kind {
        IrKind::PushInt(_, ref kind) => {
            type_stack.push(kind.into());
            ir_stack.push(ir);
        }
        IrKind::PushFloat(_, ref kind) => {
            type_stack.push(kind.into());
            ir_stack.push(ir);
        }
        IrKind::PushBool(_) => {
            ir_stack.push(ir);
            type_stack.push(Type::Bool);
        }
        IrKind::PushCStr(_) => {
            ir_stack.push(ir);
            type_stack.push(Type::Pointer(Box::new(Type::I8)));
        }
        IrKind::PushStr(_) => {
            ir_stack.push(ir);
            type_stack.push(Type::FatPointer);
        }
        IrKind::CallFn(ref ident) => {
            let (f, _) = module.resolve_ident(&ident.path)?;
            match f {
                ResolvedIdent::Function(f) => f.apply(ident, type_stack)?,
                ResolvedIdent::Module(_) => {
                    return Err(IrGenError::UnexpectedModule {
                        span: ident.path.span(),
                    })
                }
            }
            // TODO const fn
            ir_stack.push(ir);
        }
        IrKind::CallBuiltin(b) => {
            let (consumed, returns) = b.type_check(ir.span, type_stack)?;
            let can_const = ir_stack
                .iter()
                .rev()
                .take(consumed)
                .all(|i| i.kind.is_const());
            if ir_stack.len() >= consumed && can_const {
                b.apply(ir, ir_stack, &type_stack[type_stack.len() - consumed..])?;
            } else {
                ir_stack.push(ir);
            }
            let len = type_stack.len();
            type_stack.truncate(len - consumed);
            type_stack.extend_from_slice(&returns);
        }
        IrKind::Then(ref mut t) => {
            type_stack.pop_type(ir.span, &Type::Bool)?;
            if let Some(b) = ir_stack.pop_if(|x| x.kind.is_const()) {
                let IrKind::PushBool(b) = b.kind else {
                    panic!("checked by caller"); // TODO
                };

                if b {
                    // true then { 1 } else { 3 }
                    // becomes
                    // { 1 }
                    ir_stack.reserve(t.body.len());
                    for x in t.body.drain(..) {
                        update_stacks(module, x, ir_stack, type_stack)?;
                    }
                } else if t.else_thens.is_empty() {
                    // false then { 1 } else { 3 }
                    // becomes
                    // { 3 }
                    if !t.elze.is_empty() {
                        ir_stack.reserve(t.body.len());
                        for x in t.elze.drain(..) {
                            update_stacks(module, x, ir_stack, type_stack)?;
                        }
                    } else {
                        ir_stack.push(ir);
                    }
                } else {
                    // false then { 1 } else y then { 2 } else { 3 }
                    // becomes
                    // y then { 2 } else { 3 }
                    // which we then feed back to `update_stacks`.
                    let et = t.else_thens.remove(0);
                    t.body = et.body;
                    t.then_span = et.then_span;
                    for c in et.condition {
                        update_stacks(module, c, ir_stack, type_stack)?;
                    }
                    update_stacks(module, ir, ir_stack, type_stack)?;
                }
            } else {
                let mut body_ir = Vec::with_capacity(t.body.len());
                let mut after_then = type_stack.clone();
                for x in t.body.drain(..) {
                    update_stacks(module, x, &mut body_ir, &mut after_then)?;
                }
                t.body = body_ir;

                for et in &mut t.else_thens {
                    let mut body_ir = Vec::with_capacity(t.body.len());
                    let mut after_body = type_stack.clone();
                    for x in et.body.drain(..) {
                        update_stacks(module, x, &mut body_ir, &mut after_body)?;
                    }
                    if !after_then.matches(&after_body) {
                        return Err(IrGenError::IncorrectStackResults {
                            expected: after_then,
                            actual: after_body,
                            span: ir.span,
                        });
                    }
                    et.body = body_ir;
                }

                if !t.elze.is_empty() {
                    let mut body_ir = Vec::with_capacity(t.body.len());
                    let mut after_body = type_stack.clone();
                    for x in t.elze.drain(..) {
                        update_stacks(module, x, &mut body_ir, &mut after_body)?;
                    }
                    if !after_then.matches(&after_body) {
                        return Err(IrGenError::IncorrectStackResults {
                            expected: after_then,
                            actual: after_body,
                            span: ir.span,
                        });
                    }
                    t.elze = body_ir;
                }

                ir_stack.push(ir);
                type_stack.clear();
                type_stack.extend_from_slice(&after_then);
            }
        }
        IrKind::While(_) => {
            // TODO: const while
            ir_stack.push(ir);
        }
        IrKind::Break(_) => {
            ir_stack.push(ir);
        }
        IrKind::Cast(ref c) => {
            if let Some(ir) = ir_stack.pop_if(|x| x.kind.is_const()) {
                c.type_check(type_stack)?;
                c.apply(ir, ir_stack)?;
            } else {
                c.type_check(type_stack)?;
                ir_stack.push(ir);
            }
        }
    }
    Ok(())
}
