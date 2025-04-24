use std::iter::Peekable;

use miette::{Diagnostic, LabeledSpan, SourceSpan};
use thiserror::Error;

use crate::lex::{Lexer, Token, TokenKind, TokenValue};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub ident: String,
    pub len: Option<u32>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AtomKind {
    IntLit(i64),
    StrLit(String),
    CStrLit(String),
    // FloatLit(i64), // TODO

    // Ops
    Plus,
    Minus,
    Asterisk,
    Slash,

    // Keywords
    Dup,
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
            TokenValue::IntLit(n) => Ok(Self::IntLit(n)),
            TokenValue::Plus => Ok(Self::Plus),
            TokenValue::Minus => Ok(Self::Minus),
            TokenValue::Asterisk => Ok(Self::Asterisk),
            TokenValue::Slash => Ok(Self::Slash),
            TokenValue::Ellipsis => Err(()),
            TokenValue::Arrow => Err(()),
            TokenValue::Dup => Ok(Self::Dup),
            TokenValue::Drop => Ok(Self::Drop),
            TokenValue::Extern => Err(()),
            TokenValue::Fn => Err(()),
            TokenValue::Then => Err(()),
            TokenValue::Else => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Atom {
    pub token: Token,
    pub kind: AtomKind,
}

impl TryFrom<Token> for Atom {
    type Error = miette::Report;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        let kind = token
            .value
            .clone()
            .try_into()
            .map_err(|()| miette::miette!("Token is not valid atom: {:?}", token.kind()))?;
        Ok(Self { token, kind })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternFn {
    pub name: String,
    pub linker_name: String,
    pub ident: Token,
    pub args: Vec<String>,
    pub variadic: bool,
    pub returns: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fn {
    name: String,
    ident: Token,
    args: Vec<String>,
    // TODO: should we allow variadics in user-defined fns?
    // variadic: bool,
    returns: Vec<String>,
    body: Vec<Ast>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElseThen {
    else_token: Token,
    then_token: Token,
    condition: Vec<Ast>,
    body: Vec<Ast>,
}

/// ```stark
/// ... then { } else .. then { } else { }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Then {
    then_token: Token,
    body: Vec<Ast>,
    else_thens: Vec<ElseThen>,
    else_token: Token,
    else_body: Option<Vec<Ast>>,
}

// TODO: This isn't really an ast
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    Ident(Ident),
    Atom(Atom),
    ExternFn(ExternFn),
    Fn(Fn),
    Then(Then),
}

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
}

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum ParseError {
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
    UnexpectedEof { expected: String },
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
            }
            .into());
        };

        let token = token?;

        match token.value {
            TokenValue::$ident => (token, ()),
            _ => {
                return Err(miette::miette! {
                    labels = vec![LabeledSpan::new_with_span(Some("here".into()), token.span)],
                    "Unexpected token.  Expected {:?}, found {:?}", TokenKind::$ident, token.kind()
                });
            }
        }
    }};
    ($self: ident, $ident: ident(_)) => {{
        let Some(token) = $self.tokens.next() else {
            return Err(ParseError::UnexpectedEof {
                expected: "token".into(),
            }
            .into());
        };

        let token = token?;

        match token.value {
            TokenValue::$ident(ref x) => {
                let x = x.clone();
                (token, x)
            }
            _ => {
                return Err(miette::miette! {
                    labels = vec![LabeledSpan::new_with_span(Some("here".into()), token.span)],
                    "Unexpected token.  Expected {:?}, found {:?}", TokenKind::$ident, token.kind()
                });
            }
        }
    }};
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            tokens: lexer.peekable(),
        }
    }

    fn take_args(&mut self, allow_variadic: bool) -> Result<(Vec<String>, bool), miette::Report> {
        let mut args = Vec::new();
        let mut variadic = false;
        for t in &mut self.tokens {
            let t = t?;
            match t.value {
                TokenValue::Ident(ident) if !variadic => {
                    args.push(ident);
                }
                TokenValue::RParen => {
                    break;
                }
                TokenValue::Ellipsis if !variadic => {
                    if !allow_variadic {
                        return Err(ParseError::unexpected_token(
                            t,
                            &[TokenKind::Ident, TokenKind::RParen],
                        )
                        .into());
                    }
                    variadic = true;
                }
                _ => {
                    dbg!(&t);
                    return Err(ParseError::unexpected_token(
                        t,
                        if variadic {
                            &[TokenKind::RParen]
                        } else if allow_variadic {
                            &[TokenKind::Ident, TokenKind::RParen, TokenKind::Ellipsis]
                        } else {
                            &[TokenKind::Ident, TokenKind::RParen]
                        },
                    )
                    .into());
                }
            }
        }
        Ok((args, variadic))
    }

    fn take_extern_fn(&mut self) -> Result<ExternFn, miette::Report> {
        // expect_token!(self, Extern);
        expect_token!(self, Fn);
        let (ident_token, ident) = expect_token!(self, Ident(_));
        expect_token!(self, LParen);
        let (args, variadic) = self.take_args(true)?;
        expect_token!(self, Arrow);
        expect_token!(self, LParen);
        let (returns, _) = self.take_args(false)?;
        expect_token!(self, Semicolon);

        Ok(ExternFn {
            name: ident.to_string(),
            linker_name: ident,
            ident: ident_token,
            args,
            variadic,
            returns,
        })
    }

    pub fn parse(mut self) -> Result<Vec<Ast>, miette::Error> {
        let mut out = Vec::new();

        while let Some(token) = self.tokens.next() {
            let token = token?;
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

                    let (_, n) = expect_token!(self, IntLit(_));
                    let (rparen, _) = expect_token!(self, RParen);

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
                TokenValue::IntLit(_) => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Plus => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Minus => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Asterisk => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Slash => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Ellipsis => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::Arrow => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::Dup => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Drop => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Extern => {
                    let f = self.take_extern_fn()?;
                    out.push(Ast::ExternFn(f));
                }
                TokenValue::Fn => {}
                TokenValue::Then => {}
                TokenValue::Else => Err(ParseError::unexpected_token(token, &[]))?,
            }
        }

        Ok(out)
    }
}
