use std::{error::Error, fmt::Display};

use crate::lexer::{LexError, Token, TokenKind};

#[derive(Debug, Clone, Hash)]
pub enum Value {
    Int(i64),
    String(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
        }
    }
}

impl Value {
    fn add(self, rhs: Self) -> Result<Value, EvalError> {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Ok(Self::Int(a + b)),
            (Self::Int(a), Self::String(b)) => Ok(Self::String(format!("{}{}", a, b))),
            (Self::String(a), Self::Int(b)) => Ok(Self::String(format!("{}{}", a, b))),
            (Self::String(a), Self::String(b)) => Ok(Self::String(format!("{}{}", a, b))),
        }
    }

    fn sub(self, rhs: Self) -> Result<Value, EvalError> {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Ok(Self::Int(a + b)),
            (Self::Int(_), Self::String(_)) => Err(EvalError::TypeError),
            (Self::String(_), Self::Int(_)) => Err(EvalError::TypeError),
            (Self::String(_), Self::String(_)) => Err(EvalError::TypeError),
        }
    }

    fn mul(self, rhs: Self) -> Result<Value, EvalError> {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Ok(Self::Int(a * b)),
            (Self::Int(_), Self::String(_)) => Err(EvalError::TypeError),
            (Self::String(_), Self::Int(_)) => Err(EvalError::TypeError),
            (Self::String(_), Self::String(_)) => Err(EvalError::TypeError),
        }
    }

    fn div(self, rhs: Self) -> Result<Value, EvalError> {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Ok(Self::Int(a / b)),
            (Self::Int(_), Self::String(_)) => Err(EvalError::TypeError),
            (Self::String(_), Self::Int(_)) => Err(EvalError::TypeError),
            (Self::String(_), Self::String(_)) => Err(EvalError::TypeError),
        }
    }
}

pub struct Evaluator<I> {
    stack: Vec<Value>,
    tokens: I,
}

#[derive(Debug, Clone)]
pub enum EvalError {
    LexError(LexError),
    StackUnderflow { tok_start: usize, tok_end: usize },
    TypeError,
}

impl From<LexError> for EvalError {
    fn from(value: LexError) -> Self {
        Self::LexError(value)
    }
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for EvalError {
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

impl<'a, I> Evaluator<I>
where
    I: Iterator<Item = Result<Token<'a>, LexError>>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            stack: Vec::new(),
            tokens,
        }
    }

    fn pop(stack: &mut Vec<Value>, token: &Token<'_>) -> Result<Value, EvalError> {
        stack.pop().ok_or(EvalError::StackUnderflow {
            tok_start: token.offset,
            tok_end: token.end,
        })
    }

    pub fn eval(mut self) -> Result<Option<Value>, EvalError> {
        let tokens = self.tokens;
        for t in tokens {
            let t = t?;

            match t.kind {
                TokenKind::Ident(ref ident) => match &**ident {
                    "print" => {
                        let a = Self::pop(&mut self.stack, &t)?;
                        print!("{}", a);
                    }
                    "println" => {
                        let a = Self::pop(&mut self.stack, &t)?;
                        println!("{}", a);
                    }
                    _ => {
                        dbg!(&self.stack);
                        eprintln!("Ident {}", ident);
                        todo!();
                    }
                },
                TokenKind::StrLit(cow) => {
                    self.stack.push(Value::String(cow.into()));
                }
                TokenKind::IntLit(n) => {
                    self.stack.push(Value::Int(n));
                }
                TokenKind::Plus => {
                    let b = Self::pop(&mut self.stack, &t)?;
                    let a = Self::pop(&mut self.stack, &t)?;
                    self.stack.push(a.add(b)?);
                }
                TokenKind::Minus => {
                    let b = Self::pop(&mut self.stack, &t)?;
                    let a = Self::pop(&mut self.stack, &t)?;
                    self.stack.push(a.sub(b)?);
                }
                TokenKind::Asterisk => {
                    let b = Self::pop(&mut self.stack, &t)?;
                    let a = Self::pop(&mut self.stack, &t)?;
                    self.stack.push(a.mul(b)?);
                }
                TokenKind::Slash => {
                    let b = Self::pop(&mut self.stack, &t)?;
                    let a = Self::pop(&mut self.stack, &t)?;
                    self.stack.push(a.div(b)?);
                }
                TokenKind::Dup => {
                    let a = Self::pop(&mut self.stack, &t)?;
                    self.stack.push(a.clone());
                    self.stack.push(a);
                }
                TokenKind::Drop => {
                    let _ = Self::pop(&mut self.stack, &t)?;
                }
            }
        }
        Ok(self.stack.pop())
    }
}
