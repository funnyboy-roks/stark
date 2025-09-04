use std::{
    collections::HashMap,
    ffi::CString,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
};

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::{
    lex::NumLitVal,
    parse::{combine_span, Ast, AtomKind, Ident, Spanned, TypeAtom},
};

#[derive(Debug, Error, Diagnostic)]
pub enum IrGenError {
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

    #[error("Variadic arg size is incorrect.  Expected at least: {expected}  Found: {actual}")]
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
        before: TypeStack,
        after: TypeStack,
        #[label = "in this block"]
        span: SourceSpan,
    },

    #[error("Incorrect stack result in function.  Expected: {expected:?}  Actual: {actual:?}")]
    IncorrectStackResults {
        expected: TypeStack,
        actual: TypeStack,
        #[label = "in this function"]
        span: SourceSpan,
    },
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
    // TODO: make this work
    FatPointer,
    // TODO: make this work
    Struct,
    Bool,
}

#[macro_export]
macro_rules! ty {
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

                    "fatptr" => Some(Self::FatPointer), // TODO: "fatptr" is bad
                    "bool" => Some(Self::Bool),
                    _ => None,
                }
            }
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
            Type::Integer | Type::Float => 8, // assume [uif]64
            Type::Pointer(_) => 8,            // TODO: platform pointer size
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
            Type::F32 | Type::Float => 4, // TODO: Confirm
            Type::I64 | Type::U64 | Type::F64 => 8,
            Type::Integer => 8,    // assume [ui]64
            Type::Pointer(_) => 8, // TODO: platform pointer size
            Type::FatPointer => todo!("fat pointer size"),
            Type::Struct => todo!("structs"),
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
            | Type::Pointer(_) => true, // NOTE: pointers are integers according to this
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
            Type::Pointer(_) => false,
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
            Type::Pointer(_) => return None,
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
            Type::Pointer(_) => return None,
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
            Type::Pointer(_) => match other {
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
                Type::Pointer(_) => None,
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
                Type::Pointer(_) => None,
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::Pointer(_) => match other {
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
                Type::Pointer(_) => None,
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
                Type::Pointer(_) => None,
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
                Type::Pointer(_) => None,
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::Pointer(_) => match other {
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
                Type::Pointer(_) => None,
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
                Type::Pointer(_) => None,
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
                Type::Pointer(_) => None,
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::Pointer(_) => None,
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
                Type::Pointer(_) => None,
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
                Type::Pointer(_) => None,
                Type::FatPointer => None,
                Type::Struct => None,
                Type::Bool => None,
            },
            Type::Pointer(_) => None,
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
            Type::Pointer(_) => false,
            Type::FatPointer => false,
            Type::Struct => false,
            Type::Bool => false,
        }
    }

    pub fn cast_into(&self, span: SourceSpan, target: &Self) -> Result<(), IrGenError> {
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
            Type::Pointer(_) => matches!(target, Type::U64),
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
    Dup,
    Dup2,
    Swap,
    Drop,
}

impl Builtin {
    pub fn type_check(
        self,
        span: SourceSpan,
        type_stack: &[Type],
    ) -> Result<(usize, Vec<Type>), IrGenError> {
        match self {
            Builtin::Add => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.add(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot add {} to {}", b, a),
                })?;
                Ok((2, returns))
            }
            Builtin::Sub => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.sub(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot subtract {} from {}", b, a),
                })?;
                Ok((2, returns))
            }
            Builtin::Mul => {
                let [b, a] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                let returns = a.mul(b).ok_or_else(|| IrGenError::TypeError2 {
                    span,
                    message: format!("Cannot multiply {} by {}", b, a),
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
            Builtin::Dup => {
                let a = type_stack
                    .last()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                Ok((1, vec![a.clone(), a.clone()]))
            }
            Builtin::Dup2 => {
                let [a, b] = type_stack
                    .last_chunk::<2>()
                    .ok_or(IrGenError::StackUnderflow { span })?;
                Ok((2, vec![a.clone(), b.clone(), a.clone(), b.clone()]))
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
                assert!(a_ty.add(b_ty).is_some());

                let spans = [b_ir.span, a_ir.span, current.span];
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
                        let (IrKind::PushInt(b), IrKind::PushInt(a)) = (&b_ir.kind, &a_ir.kind)
                        else {
                            panic!("checked by caller")
                        };
                        ir_stack.push(Ir::new_spans(spans, IrKind::PushInt(*a + *b)))
                    }
                    Type::F32 | Type::F64 | Type::Float => {
                        let (IrKind::PushFloat(b), IrKind::PushFloat(a)) = (&b_ir.kind, &a_ir.kind)
                        else {
                            panic!("checked by caller")
                        };
                        ir_stack.push(Ir::new_spans(spans, IrKind::PushFloat(*a + *b)))
                    }
                    Type::Pointer(_) => todo!(),
                    Type::FatPointer => todo!(),
                    Type::Struct => todo!(),
                    Type::Bool => todo!(),
                };

                Ok(())
            }
            Builtin::Sub => todo!(),
            Builtin::Mul => todo!(),
            Builtin::Div => todo!(),
            Builtin::Mod => todo!(),
            Builtin::Equal => {
                let [a_ty, b_ty] = type_stack else {
                    unreachable!("checked by caller")
                };
                let b_ir = ir_stack.pop().expect("checked by caller");
                let a_ir = ir_stack.pop().expect("checked by caller");
                assert!(a_ty.add(b_ty).is_some());

                let spans = [b_ir.span, a_ir.span, current.span];
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
                        let (IrKind::PushInt(b), IrKind::PushInt(a)) = (&b_ir.kind, &a_ir.kind)
                        else {
                            panic!("checked by caller")
                        };
                        ir_stack.push(Ir::new_spans(spans, IrKind::PushBool(*a == *b)))
                    }
                    Type::F32 | Type::F64 | Type::Float => {
                        let (IrKind::PushFloat(b), IrKind::PushFloat(a)) = (&b_ir.kind, &a_ir.kind)
                        else {
                            panic!("checked by caller")
                        };
                        ir_stack.push(Ir::new_spans(spans, IrKind::PushBool(*a == *b)))
                    }
                    Type::Pointer(_) => todo!(),
                    Type::FatPointer => todo!(),
                    Type::Struct => todo!(),
                    Type::Bool => {
                        let (IrKind::PushBool(b), IrKind::PushBool(a)) = (&b_ir.kind, &a_ir.kind)
                        else {
                            panic!("checked by caller")
                        };
                        ir_stack.push(Ir::new_spans(spans, IrKind::PushBool(*a == *b)))
                    }
                };
                Ok(())
            }
            Builtin::Not => {
                let b_ir = ir_stack.pop().unwrap();
                let IrKind::PushBool(b) = b_ir.kind else {
                    panic!("checked by caller")
                };
                ir_stack.push(Ir::new_spans(
                    [current.span, b_ir.span],
                    IrKind::PushBool(!b),
                ));
                Ok(())
            }
            Builtin::NotEqual => todo!(),
            Builtin::Lt => todo!(),
            Builtin::Lte => todo!(),
            Builtin::Gt => todo!(),
            Builtin::Gte => todo!(),
            Builtin::Dup => {
                ir_stack.extend_from_within(ir_stack.len() - 1..);
                Ok(())
            }
            Builtin::Dup2 => {
                ir_stack.extend_from_within(ir_stack.len() - 2..);
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
        }
    }
}

#[derive(Debug, Clone)]
pub struct WhileIr {
    pub loop_id: String,
    pub condition: Vec<Ir>,
    pub condition_span: SourceSpan,
    pub body: Vec<Ir>,
    pub body_span: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct ElseThenIr {
    pub condition: Vec<Ir>,
    pub body: Vec<Ir>,
}

#[derive(Debug, Clone)]
pub struct ThenIr {
    pub then_span: SourceSpan,
    pub body: Vec<Ir>,
    pub else_thens: Vec<ElseThenIr>,
    pub elze: Vec<Ir>,
}

#[derive(Debug, Clone)]
pub struct Cast(Type);

impl Cast {
    pub fn apply(&self, mut ir: Ir, ir_stack: &mut Vec<Ir>) -> Result<(), IrGenError> {
        match ir.kind {
            IrKind::PushInt(ref mut n) => {
                if self.0.is_integer() {
                    *n %= 2u32.pow(8 * self.0.size()) as i128;
                    ir_stack.push(ir);
                } else if self.0 == Type::Bool {
                    ir_stack.push(Ir::new(ir.span, IrKind::PushBool(*n != 0)));
                } else if self.0.is_float() {
                    let f = *n as f64;
                    ir_stack.push(Ir::new(ir.span, IrKind::PushFloat(f)));
                } else {
                    panic!()
                }
            }
            IrKind::PushFloat(f) => {
                if self.0.is_integer() {
                    let n = f as i128 % 2u32.pow(8 * self.0.size()) as i128;
                    ir_stack.push(Ir::new(ir.span, IrKind::PushInt(n)));
                } else if self.0.is_float() {
                    // nop
                    ir_stack.push(ir);
                } else {
                    panic!()
                }
            }
            IrKind::PushBool(_) => {
                assert_eq!(self.0, Type::Bool);
                ir_stack.push(ir);
            }
            IrKind::PushCStr(_) => todo!(),
            IrKind::PushStr(_) => todo!(),
            _ => panic!("checked by compiler"),
        }

        Ok(())
    }

    pub fn type_check(
        &self,
        span: SourceSpan,
        type_stack: &mut TypeStack,
    ) -> Result<(), IrGenError> {
        let ty = type_stack.pop(span)?;
        ty.cast_into(span, &self.0)?;
        type_stack.push(self.0.clone());
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum IrKind {
    /// Integer literal, to be coerced / operated on by const fns
    PushInt(i128),
    /// Float literal, to be coerced / operated on by const fns
    PushFloat(f64),
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
            IrKind::PushInt(_) | IrKind::PushFloat(_) | IrKind::PushBool(_) => true,
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
    pub span: SourceSpan,
    pub kind: IrKind,
}

impl Ir {
    fn new(span: SourceSpan, kind: IrKind) -> Self {
        Self { span, kind }
    }

    fn new_spans(spans: impl IntoIterator<Item = SourceSpan>, kind: IrKind) -> Self {
        Self {
            span: combine_span(spans),
            kind,
        }
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
    pub ident_span: SourceSpan,
    pub args: TypeStack,
    pub variadic: bool,
    pub returns: TypeStack,
    pub external: bool,
}

impl FunctionSignature {
    pub fn apply(&self, ident: &Ident, type_stack: &mut TypeStack) -> Result<(), IrGenError> {
        if let Some(len) = ident.len {
            if self.variadic && (len as usize) < self.args.len() {
                return Err(IrGenError::IncorrectExplicitVariadicArgSize {
                    expected: self.args.len(),
                    actual: len as usize,
                    span: ident.span,
                });
            } else if !self.variadic && self.args.len() != len as usize {
                return Err(IrGenError::IncorrectExplicitArgSize {
                    expected: self.args.len(),
                    actual: len as usize,
                    span: ident.span,
                });
            }
        }
        for expected in self.args.iter().rev() {
            // TODO: error for all args at the same time
            type_stack.pop_type(ident.span, expected)?;
        }
        if let Some(len) = ident.len
            && len as usize > self.args.len()
            && self.variadic
        {
            for _ in 0..(len as usize - self.args.len()) {
                type_stack.pop(ident.span)?;
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

    pub fn pop(&mut self, span: SourceSpan) -> Result<Type, IrGenError> {
        self.inner.pop().ok_or(IrGenError::StackUnderflow { span })
    }

    pub fn pop_type(&mut self, span: SourceSpan, expected: &Type) -> Result<(), IrGenError> {
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

// TODO: names are very hard
#[derive(Debug, Default)]
pub struct Module {
    /// The ASTs for the module
    asts: Vec<Ast>,

    pub functions: HashMap<String, FunctionSignature>,
    pub converted_functions: Vec<ConvertedFunction>,
}

impl Module {
    pub fn new(asts: Vec<Ast>) -> Self {
        Self {
            asts,
            functions: Default::default(),
            converted_functions: Default::default(),
        }
    }

    pub fn compile_module(&mut self) -> Result<(), IrGenError> {
        self.scan_functions()?;
        for ast in std::mem::take(&mut self.asts) {
            self.module_ast(ast)?;
        }
        Ok(())
    }

    fn scan_functions(&mut self) -> Result<(), IrGenError> {
        for ast in &self.asts {
            match ast {
                Ast::Ident(_) | Ast::Atom(_) | Ast::Then(_) | Ast::While(_) | Ast::Cast(_) => {
                    continue
                }
                Ast::ExternFn(f) => {
                    if let Some(original) = self.functions.get(&f.name) {
                        return Err(IrGenError::RepeatDefinition {
                            ident: f.name.clone(),
                            original: original.ident_span,
                            repeat: f.ident.span,
                        });
                    }
                    self.functions.insert(
                        f.name.clone(),
                        FunctionSignature {
                            name: f.name.clone(),
                            linker_name: f.linker_name.clone(),
                            ident_span: f.ident.span,
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
                            repeat: f.ident.span,
                        });
                    }
                    self.functions.insert(
                        f.name.clone(),
                        FunctionSignature {
                            name: f.name.clone(),
                            linker_name: f.name.clone(), // TODO: support custom linker name
                            ident_span: f.ident.span,
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
            }
        }
        Ok(())
    }

    fn compile_body(
        &mut self,
        type_stack: &mut TypeStack,
        out: &mut Vec<Ir>,
        outer_loop: Option<&(String, TypeStack, SourceSpan)>,
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
        outer_loop: Option<&(String, TypeStack, SourceSpan)>,
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
                    Builtin::$builtin.type_check($atom.token.span, type_stack)?;
                let len = type_stack.len();
                type_stack.truncate(len - consumed);
                type_stack.extend_from_slice(&returns);
                out.push(Ir::new(
                    $atom.token.span,
                    IrKind::CallBuiltin(Builtin::$builtin),
                ));
            }};
        }
        eprintln!("type_stack = {:?}", type_stack);
        match ast {
            Ast::Ident(ident) => {
                let f = self.functions.get(&ident.ident).ok_or_else(|| {
                    IrGenError::UndefinedSymbol {
                        ident: ident.ident.clone(),
                        span: ident.span,
                    }
                })?;
                f.apply(&ident, type_stack)?;
                out.push(Ir::new(ident.span, IrKind::CallFn(ident)));
            }
            Ast::Atom(atom) => match atom.kind {
                AtomKind::NumLit(num_lit) => match num_lit.value {
                    NumLitVal::Integer(n) => {
                        simple!(atom.token.span, ty!(Integer), IrKind::PushInt(n))
                    }
                    NumLitVal::Float(f) => {
                        simple!(atom.token.span, ty!(Float), IrKind::PushFloat(f))
                    }
                },
                AtomKind::BoolLit(b) => simple!(atom.token.span, ty!(bool), IrKind::PushBool(b)),
                AtomKind::StrLit(s) => {
                    simple!(atom.token.span, Type::FatPointer, IrKind::PushStr(s));
                }
                AtomKind::CStrLit(s) => simple!(atom.token.span, ty!(*i8), IrKind::PushCStr(s)),
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
                AtomKind::Dup => builtin!(atom, Dup),
                AtomKind::Dup2 => builtin!(atom, Dup2),
                AtomKind::Swap => builtin!(atom, Swap),
                AtomKind::Drop => builtin!(atom, Drop),
                AtomKind::Break => {
                    if let Some((loop_id, start_ts, body_span)) = outer_loop {
                        if !type_stack.matches(start_ts) {
                            return Err(IrGenError::StackChanged {
                                span: *body_span,
                                before: start_ts.clone(),
                                after: type_stack.clone(),
                            });
                        }
                        out.push(Ir::new(atom.token.span, IrKind::Break(loop_id.clone())));
                    } else {
                        todo!("Break not in a loop")
                    }
                }
            },
            Ast::ExternFn(_) => {} // TODO
            Ast::Fn(_) => todo!("nested function"),
            Ast::Then(t) => {
                type_stack.pop_type(t.then_token.span, &Type::Bool)?;

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
                    let mut condition = Vec::with_capacity(et.condition.len());
                    self.compile_body(type_stack, &mut condition, outer_loop, et.condition)?;
                    type_stack.pop_type(et.then_token.span, &Type::Bool)?;

                    let mut body = Vec::with_capacity(et.body.len());
                    self.compile_body(type_stack, &mut body, outer_loop, et.body)?;
                    if !type_stack.matches(&after_then) {
                        return Err(IrGenError::StackChanged {
                            before: after_then,
                            after: type_stack.clone(),
                            span: et.then_token.span,
                        });
                    }
                    // reset type stack
                    type_stack.clear();
                    type_stack.extend_from_slice(&before);

                    else_thens.push(ElseThenIr { condition, body });
                }

                let elze = if let Some((elze_body, token)) = t.elze {
                    let mut body = Vec::with_capacity(elze_body.len());
                    self.compile_body(type_stack, &mut body, outer_loop, elze_body)?;
                    if !type_stack.matches(&after_then) {
                        return Err(IrGenError::StackChanged {
                            before: after_then,
                            after: type_stack.clone(),
                            span: token.span,
                        });
                    }
                    type_stack.clear();
                    type_stack.extend_from_slice(&before);
                    body
                } else {
                    Vec::new()
                };
                type_stack.clear();
                type_stack.extend_from_slice(&after_then);

                out.push(Ir::new(
                    t.then_token.span,
                    IrKind::Then(ThenIr {
                        then_span: t.then_token.span,
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
                let loop_id = format!("while_{}", span.offset());
                self.compile_body(type_stack, &mut condition, None, w.condition)?;

                // bool should have been added to the stack
                type_stack.pop_type(w.while_token.span, &Type::Bool)?;

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
                let x = Cast(Type::from_atom(&c.target).expect("TODO: type errors"));
                x.type_check(c.span(), type_stack)?;
                out.push(Ir::new(c.span(), IrKind::Cast(x)));
            }
        }
        Ok(())
    }

    fn module_ast(&mut self, ast: Ast) -> Result<(), IrGenError> {
        match ast {
            Ast::Ident(_) => todo!("unexpected ident"),
            Ast::Atom(_) => todo!("unexpected atom"),
            Ast::ExternFn(_) => {} // TODO
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
                        span: f.ident.span,
                    });
                }
            }
            Ast::Then(_) => todo!(),
            Ast::While(_) => todo!(),
            Ast::Cast(_) => todo!(),
        }
        Ok(())
    }

    fn optimise(&self, ir: Vec<Ir>, type_stack: &mut TypeStack) -> Result<Vec<Ir>, IrGenError> {
        let mut out = Vec::with_capacity(ir.len());

        for ir in ir {
            update_stacks(&self.functions, ir, &mut out, type_stack)?;
        }

        Ok(out)
    }
}

pub fn update_stacks(
    functions: &HashMap<String, FunctionSignature>,
    mut ir: Ir,
    ir_stack: &mut Vec<Ir>,
    type_stack: &mut TypeStack,
) -> Result<(), IrGenError> {
    match ir.kind {
        IrKind::PushInt(_) => {
            ir_stack.push(ir);
            type_stack.push(Type::Integer);
        }
        IrKind::PushFloat(_) => {
            ir_stack.push(ir);
            type_stack.push(Type::Float);
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
            let f = functions
                .get(&ident.ident)
                .ok_or_else(|| IrGenError::UndefinedSymbol {
                    ident: ident.ident.clone(),
                    span: ir.span,
                })?;
            f.apply(ident, type_stack)?;
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
                        update_stacks(functions, x, ir_stack, type_stack)?;
                    }
                } else if t.else_thens.is_empty() {
                    // false then { 1 } else { 3 }
                    // becomes
                    // { 3 }
                    if !t.elze.is_empty() {
                        ir_stack.reserve(t.body.len());
                        for x in t.body.drain(..) {
                            update_stacks(functions, x, ir_stack, type_stack)?;
                        }
                    } else {
                        ir_stack.push(ir);
                    }
                } else {
                    // false then { 1 } else y then { 2 } else { 3 }
                    // becomes
                    // y then { 2 } else { 3 }
                    let et = t.else_thens.remove(0);
                    t.body = et.body;
                    ir_stack.push(ir);
                }
            } else {
                let mut body_ir = Vec::with_capacity(t.body.len());
                let mut after_then = type_stack.clone();
                for x in t.body.drain(..) {
                    update_stacks(functions, x, &mut body_ir, &mut after_then)?;
                }
                t.body = body_ir;

                for et in &mut t.else_thens {
                    let mut body_ir = Vec::with_capacity(t.body.len());
                    let mut after_body = type_stack.clone();
                    for x in et.body.drain(..) {
                        update_stacks(functions, x, &mut body_ir, &mut after_body)?;
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
                        update_stacks(functions, x, &mut body_ir, &mut after_body)?;
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
                c.type_check(ir.span, type_stack)?;
                c.apply(ir, ir_stack)?;
            } else {
                c.type_check(ir.span, type_stack)?;
                ir_stack.push(ir);
            }
        }
    }
    Ok(())
}
