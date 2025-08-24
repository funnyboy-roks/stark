use std::{ffi::CString, iter::Peekable};

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::lex::{LexError, Lexer, NumLit, NumLitVal, Token, TokenKind, TokenValue};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub ident: String,
    pub len: Option<u32>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAtom {
    Ident(String),
    Pointer(Box<TypeAtom>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AtomKind {
    NumLit(NumLit),
    StrLit(String),
    CStrLit(CString),
    Type(TypeAtom),
    // FloatLit(i64), // TODO

    // Ops
    Plus,
    Minus,
    Asterisk,
    Slash,
    Equal,
    Not,
    Lt,
    Percent,

    // Keywords
    Dup,
    Dup2,
    Swap,
    Drop,
}

impl TryFrom<TokenValue> for AtomKind {
    type Error = ();

    fn try_from(value: TokenValue) -> Result<Self, Self::Error> {
        match value {
            TokenValue::Ident(_) => Err(()),
            TokenValue::LParen => Err(()),
            TokenValue::RParen => Err(()),
            TokenValue::LCurly => Err(()),
            TokenValue::RCurly => Err(()),
            TokenValue::Semicolon => Err(()),
            TokenValue::StrLit(s) => Ok(Self::StrLit(s)),
            TokenValue::CStrLit(s) => Ok(Self::CStrLit(s)),
            TokenValue::NumLit(n) => Ok(Self::NumLit(n)),
            TokenValue::Plus => Ok(Self::Plus),
            TokenValue::Minus => Ok(Self::Minus),
            TokenValue::Asterisk => Ok(Self::Asterisk),
            TokenValue::Slash => Ok(Self::Slash),
            TokenValue::Equal => Ok(Self::Equal),
            TokenValue::Not => Ok(Self::Not),
            TokenValue::Lt => Ok(Self::Lt),
            TokenValue::Percent => Ok(Self::Percent),
            TokenValue::Ellipsis => Err(()),
            TokenValue::Arrow => Err(()),
            TokenValue::Dup => Ok(Self::Dup),
            TokenValue::Dup2 => Ok(Self::Dup2),
            TokenValue::Swap => Ok(Self::Swap),
            TokenValue::Drop => Ok(Self::Drop),
            TokenValue::Extern => Err(()),
            TokenValue::Fn => Err(()),
            TokenValue::Then => Err(()),
            TokenValue::Else => Err(()),
            TokenValue::While => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Atom {
    pub token: Token,
    pub kind: AtomKind,
}

impl TryFrom<Token> for Atom {
    type Error = ParseError;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        // TODO: remove this unwrap
        let kind = token.value.clone().try_into().unwrap();
        Ok(Self { token, kind })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternFn {
    pub name: String,
    pub linker_name: String,
    pub ident: Token,
    pub args: Vec<TypeAtom>,
    pub variadic: bool,
    pub returns: Vec<TypeAtom>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fn {
    pub name: String,
    pub ident: Token,
    pub args: Vec<TypeAtom>,
    // TODO: should we allow variadics in user-defined fns?
    // variadic: bool,
    pub returns: Vec<TypeAtom>,
    pub body: Vec<Ast>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseThen {
    pub else_token: Token,
    pub then_token: Token,
    pub condition: Vec<Ast>,
    pub body: Vec<Ast>,
}

/// ```stark
/// ... then { } else .. then { } else { }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Then {
    pub then_token: Token,
    pub body: Vec<Ast>,
    pub else_thens: Vec<ElseThen>,
    pub elze: Option<(Vec<Ast>, Token)>,
}

/// ```stark
/// ... while x y z { }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub while_token: Token,
    pub condition: Vec<Ast>,
    pub body: Vec<Ast>,
}

// TODO: This isn't really an ast
#[derive(Debug, Clone, PartialEq)]
pub enum Ast {
    Ident(Ident),
    Atom(Atom),
    ExternFn(ExternFn),
    Fn(Fn),
    Then(Then),
    While(While),
}

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
}

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum ParseError {
    #[error(transparent)]
    LexError(#[from] LexError),
    #[error("Unexpected token '{found:?}'")]
    UnexpectedToken {
        #[label = "here"]
        span: SourceSpan,
        found: TokenKind,
    },
    #[error("Unexpected token '{found:?}' Expected '{expected:?}'")]
    UnexpectedTokenExpected {
        #[label = "here"]
        span: SourceSpan,
        found: TokenKind,
        expected: TokenKind,
    },
    #[error("Unexpected token '{found:?}' Expected one of '{expected:?}'")]
    UnexpectedTokenExpectedMany {
        #[label = "here"]
        span: SourceSpan,
        found: TokenKind,
        expected: &'static [TokenKind],
    },
    #[error("Expected {expected}, found EOF")]
    UnexpectedEof {
        expected: String,
        #[label = "while matching this"]
        matching: Option<SourceSpan>,
    },
    #[error("Expected integer, found float value {found}")]
    ExpectedInteger {
        #[label = "here"]
        span: SourceSpan,
        found: f32,
    },
    #[error("Nested function definitions are not supported")]
    NestedFunction {
        #[label = "Function definition"]
        span: SourceSpan,
    },
}

impl ParseError {
    pub fn unexpected_token(token: Token, expected: &'static [TokenKind]) -> Self {
        match expected {
            [] => Self::UnexpectedToken {
                span: token.span,
                found: token.kind(),
            },
            [expected] => Self::UnexpectedTokenExpected {
                span: token.span,
                found: token.kind(),
                expected: *expected,
            },
            _ => Self::UnexpectedTokenExpectedMany {
                span: token.span,
                found: token.kind(),
                expected,
            },
        }
    }
}

macro_rules! expect_token {
    ($self: ident, $ident: ident) => {{
        let Some(token) = $self.tokens.next() else {
            return Err(ParseError::UnexpectedEof {
                expected: "token".into(),
                matching: None,
            }
            .into());
        };

        let token = token?;

        match token.value {
            TokenValue::$ident => (token, ()),
            _ => {
                return Err(ParseError::unexpected_token(token, &[TokenKind::$ident]));
            }
        }
    }};
    ($self: ident, $ident: ident(_)) => {{
        let Some(token) = $self.tokens.next() else {
            return Err(ParseError::UnexpectedEof {
                expected: "token".into(),
                matching: None,
            });
        };

        let token = token?;

        match token.value {
            TokenValue::$ident(ref x) => {
                let x = x.clone();
                (token, x)
            }
            _ => {
                return Err(ParseError::unexpected_token(token, &[TokenKind::$ident]));
            }
        }
    }};
}

macro_rules! try_expect_token {
    ($self: ident, $ident: ident) => {
        'a: {
            let Some(token) = $self.tokens.peek() else {
                break 'a None;
            };

            let Ok(token) = token.as_ref() else {
                break 'a None;
            };

            match token.value {
                TokenValue::$ident => {
                    let token = $self.tokens.next().expect("checked above")?;
                    Some(token)
                }
                _ => None,
            }
        }
    };
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            tokens: lexer.peekable(),
        }
    }

    fn take_type(&mut self) -> Result<TypeAtom, ParseError> {
        let Some(token) = self.tokens.next() else {
            return Err(ParseError::UnexpectedEof {
                expected: "type".into(),
                matching: None,
            });
        };

        let token = token?;
        match token.value {
            TokenValue::Asterisk => {
                let inner = self.take_type()?;
                Ok(TypeAtom::Pointer(Box::new(inner)))
            }
            TokenValue::Ident(ident) => Ok(TypeAtom::Ident(ident)),
            _ => {
                return Err(ParseError::unexpected_token(
                    token,
                    &[TokenKind::Ident, TokenKind::Asterisk],
                ))
            }
        }
    }

    fn take_args(&mut self, allow_variadic: bool) -> Result<(Vec<TypeAtom>, bool), ParseError> {
        let mut args = Vec::new();
        let mut variadic = false;
        while let Some(t) = self.tokens.next() {
            let t = t?;
            match t.value {
                TokenValue::Ident(ident) if !variadic => {
                    args.push(TypeAtom::Ident(ident));
                }
                TokenValue::Asterisk => {
                    let ty = self.take_type()?;
                    args.push(TypeAtom::Pointer(Box::new(ty)));
                }
                TokenValue::RParen => {
                    break;
                }
                TokenValue::Ellipsis if !variadic => {
                    if !allow_variadic {
                        return Err(ParseError::unexpected_token(
                            t,
                            &[TokenKind::Ident, TokenKind::RParen],
                        ));
                    }
                    variadic = true;
                }
                _ => {
                    return Err(ParseError::unexpected_token(
                        t,
                        if variadic {
                            &[TokenKind::RParen]
                        } else if allow_variadic {
                            &[TokenKind::Ident, TokenKind::RParen, TokenKind::Ellipsis]
                        } else {
                            &[TokenKind::Ident, TokenKind::RParen]
                        },
                    ));
                }
            }
        }
        Ok((args, variadic))
    }

    fn take_extern_fn(&mut self) -> Result<ExternFn, ParseError> {
        // expect_token!(self, Extern);
        expect_token!(self, Fn);
        let (ident_token, ident) = expect_token!(self, Ident(_));

        let Some(next) = self.tokens.peek() else {
            return Err(ParseError::UnexpectedEof {
                expected: "token".into(),
                matching: None,
            });
        };
        let next = next.as_ref().map_err(|e| e.clone())?;
        let (args, variadic) = match next.kind() {
            TokenKind::LParen => {
                expect_token!(self, LParen);
                self.take_args(true)?
            }
            TokenKind::Arrow => (Vec::new(), false),
            TokenKind::Semicolon => (Vec::new(), false),
            _ => {
                return Err(ParseError::unexpected_token(
                    self.tokens.next().unwrap().expect("error checked above"),
                    &[TokenKind::LCurly, TokenKind::Arrow],
                ));
            }
        };

        let Some(next) = self.tokens.next() else {
            return Err(ParseError::UnexpectedEof {
                expected: "token".into(),
                matching: None,
            });
        };
        let next = next?;
        let returns = match next.kind() {
            TokenKind::Semicolon => Vec::new(),
            TokenKind::Arrow => {
                expect_token!(self, LParen);
                let (returns, _) = self.take_args(false)?;
                expect_token!(self, Semicolon);
                returns
            }
            _ => {
                return Err(ParseError::unexpected_token(
                    next,
                    &[TokenKind::LCurly, TokenKind::Arrow],
                ));
            }
        };

        Ok(ExternFn {
            name: ident.to_string(),
            linker_name: ident,
            ident: ident_token,
            args,
            variadic,
            returns,
        })
    }

    fn take_fn(&mut self) -> Result<Fn, ParseError> {
        // expect_token!(self, Extern);
        // expect_token!(self, Fn);
        let (ident_token, ident) = expect_token!(self, Ident(_));

        let Some(next) = self.tokens.peek() else {
            return Err(ParseError::UnexpectedEof {
                expected: "token".into(),
                matching: None,
            });
        };
        let next = next.as_ref().map_err(|e| e.clone())?;
        let (args, _) = match next.kind() {
            TokenKind::LParen => {
                expect_token!(self, LParen);
                // TODO: allow variadic?
                self.take_args(false)?
            }
            TokenKind::Arrow => (Vec::new(), false),
            TokenKind::LCurly => (Vec::new(), false),
            _ => {
                return Err(ParseError::unexpected_token(
                    self.tokens.next().unwrap().expect("error checked above"),
                    &[TokenKind::LCurly, TokenKind::Arrow],
                ));
            }
        };

        let Some(next) = self.tokens.next() else {
            return Err(ParseError::UnexpectedEof {
                expected: "token".into(),
                matching: None,
            });
        };
        let next = next?;
        let (returns, body_open) = match next.kind() {
            TokenKind::LCurly => (Vec::new(), next),
            TokenKind::Arrow => {
                expect_token!(self, LParen);
                let (returns, _) = self.take_args(false)?;
                let (body_open, ()) = expect_token!(self, LCurly);
                (returns, body_open)
            }
            _ => {
                return Err(ParseError::unexpected_token(
                    next,
                    &[TokenKind::LCurly, TokenKind::Arrow],
                ));
            }
        };

        let (body, _) = self.take_body(TokenKind::RCurly, Some(&body_open))?;

        Ok(Fn {
            name: ident.to_string(),
            ident: ident_token,
            args,
            returns,
            body,
        })
    }

    // this is very similar to parse, maybe we want to combine them?
    fn take_body(
        &mut self,
        end_token: TokenKind,
        open_token: Option<&Token>,
    ) -> Result<(Vec<Ast>, Token), ParseError> {
        let mut out = Vec::new();

        while let Some(token) = self.tokens.next() {
            let token = token?;
            if token.kind() == end_token {
                return Ok((out, token));
            }
            match token.value {
                TokenValue::Ident(ref s) => 'x: {
                    let s = s.to_string();
                    if let Some(Ok(x)) = self.tokens.peek() {
                        if x.kind() != TokenKind::LParen {
                            out.push(Ast::Ident(Ident {
                                ident: s,
                                len: None,
                                span: token.span,
                            }));
                            break 'x;
                        }
                    }
                    self.tokens.next().unwrap()?;

                    let (num, n) = expect_token!(self, NumLit(_));
                    let (rparen, _) = expect_token!(self, RParen);

                    let n = match n.value {
                        NumLitVal::Integer(n) => n,
                        NumLitVal::Float(val) => {
                            return Err(ParseError::ExpectedInteger {
                                span: num.span,
                                found: val,
                            })
                        }
                    };

                    out.push(Ast::Ident(Ident {
                        ident: s,
                        len: Some(n as u32),
                        span: (token.span.offset()..rparen.span.offset() + rparen.span.len())
                            .into(),
                    }));
                }
                TokenValue::LParen => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::RParen => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::LCurly => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::RCurly => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::Semicolon => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::StrLit(_) => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::CStrLit(_) => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::NumLit(_) => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Plus => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Minus => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Asterisk => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Slash => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Equal => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Not => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Lt => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Percent => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Ellipsis => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::Arrow => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::Dup => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Dup2 => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Swap => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Drop => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Extern => Err(ParseError::NestedFunction { span: token.span })?,
                TokenValue::Fn => Err(ParseError::NestedFunction { span: token.span })?,
                TokenValue::Then => {
                    let t = self.take_then(token)?;
                    out.push(Ast::Then(t));
                }
                TokenValue::Else => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::While => Err(ParseError::unexpected_token(token, &[]))?,
            }
        }

        Err(ParseError::UnexpectedEof {
            expected: format!("{:?}", end_token),
            matching: open_token.map(|t| t.span),
        })
    }

    fn take_then(&mut self, then_token: Token) -> Result<Then, ParseError> {
        // expect_token!(self, Then);
        expect_token!(self, LCurly);
        let (body, _) = self.take_body(TokenKind::RCurly, Some(&then_token))?;

        let mut else_thens = Vec::new();

        loop {
            let Some(else_token) = try_expect_token!(self, Else) else {
                return Ok(Then {
                    then_token,
                    body,
                    else_thens,
                    elze: None,
                });
            };

            match try_expect_token!(self, LCurly) {
                Some(l) => {
                    let (else_body, _) = self.take_body(TokenKind::RCurly, Some(&l))?;
                    return Ok(Then {
                        then_token,
                        body,
                        else_thens,
                        elze: Some((else_body, else_token)),
                    });
                }
                None => {
                    let (condition, then_token) = self.take_body(TokenKind::Then, None)?;
                    let (l, ()) = expect_token!(self, LCurly);
                    let (body, _) = self.take_body(TokenKind::RCurly, Some(&l))?;
                    else_thens.push(ElseThen {
                        else_token,
                        then_token,
                        condition,
                        body,
                    });
                }
            }
        }
    }

    fn take_while(&mut self, while_token: Token) -> Result<While, ParseError> {
        // expect_token!(self, Then);
        let (condition, l_curly) = self.take_body(TokenKind::LCurly, Some(&while_token))?;
        let (body, _) = self.take_body(TokenKind::RCurly, Some(&l_curly))?;

        Ok(While {
            while_token,
            condition,
            body,
        })
    }

    pub fn parse(mut self) -> Result<Vec<Ast>, ParseError> {
        let mut out = Vec::new();

        while let Some(token) = self.tokens.next() {
            let token = token?;
            match token.value {
                TokenValue::Ident(ref ident) => 'x: {
                    let ident = ident.to_string();
                    if let Some(Ok(x)) = self.tokens.peek() {
                        if x.kind() != TokenKind::LParen {
                            out.push(Ast::Ident(Ident {
                                ident,
                                len: None,
                                span: token.span,
                            }));
                            break 'x;
                        }
                    } else {
                        out.push(Ast::Ident(Ident {
                            ident,
                            len: None,
                            span: token.span,
                        }));
                        break 'x;
                    }
                    self.tokens.next().unwrap()?;

                    let (num, n) = expect_token!(self, NumLit(_));
                    let (rparen, _) = expect_token!(self, RParen);

                    let n = match n.value {
                        NumLitVal::Integer(n) => n,
                        NumLitVal::Float(val) => {
                            return Err(ParseError::ExpectedInteger {
                                span: num.span,
                                found: val,
                            })
                        }
                    };

                    out.push(Ast::Ident(Ident {
                        ident,
                        len: Some(n as u32),
                        span: (token.span.offset()..rparen.span.offset() + rparen.span.len())
                            .into(),
                    }));
                }
                TokenValue::LParen => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::RParen => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::LCurly => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::RCurly => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::Semicolon => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::StrLit(_) => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::CStrLit(_) => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::NumLit(_) => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Plus => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Minus => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Asterisk => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Slash => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Not => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Lt => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Percent => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Equal => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Ellipsis => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::Arrow => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::Dup => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Dup2 => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Swap => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Drop => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Extern => {
                    let f = self.take_extern_fn()?;
                    out.push(Ast::ExternFn(f));
                }
                TokenValue::Fn => {
                    let f = self.take_fn()?;
                    out.push(Ast::Fn(f));
                }
                TokenValue::Then => {
                    let t = self.take_then(token)?;
                    out.push(Ast::Then(t));
                }
                TokenValue::Else => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::While => {
                    out.push(Ast::While(self.take_while(token)?));
                }
            }
        }

        Ok(out)
    }
}
