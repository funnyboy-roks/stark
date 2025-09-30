use std::{
    ffi::CString,
    iter::Peekable,
    ops::{Deref, DerefMut},
};

use miette::Diagnostic;
use thiserror::Error;

use crate::{
    lex::{LexError, Lexer, NumLit, NumLitVal, Token, TokenKind, TokenValue},
    span::{Span, Spanned},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathElement {
    pub name: String,
    span: Span,
}

impl Spanned for PathElement {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Path {
    inner: Vec<PathElement>,
    /// This path starts with `::`
    pub is_root: bool,
    span: Span,
}

impl Path {
    pub fn from_vec(vec: Vec<PathElement>, is_root: bool) -> Self {
        let span = vec.iter().map(|e| e.span()).sum();
        Self {
            inner: vec,
            span,
            is_root,
        }
    }

    pub fn last(&self) -> &PathElement {
        assert!(
            self.len() >= 1,
            "Path should always contain at least one elemnt"
        );
        &self[self.len() - 1]
    }
}

impl Spanned for Path {
    fn span(&self) -> Span {
        self.span
    }
}

impl std::fmt::Debug for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, elt) in self.inner.iter().enumerate() {
            if i != 0 && !self.is_root {
                f.write_str("::")?;
            }
            f.write_str(&elt.name)?;
        }
        Ok(())
    }
}

impl Deref for Path {
    type Target = Vec<PathElement>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Path {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl AsRef<[PathElement]> for Path {
    fn as_ref(&self) -> &[PathElement] {
        &self.inner
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub path: Path,
    pub arity: Option<u32>,
    span: Span,
}

impl Spanned for Ident {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAtom {
    Ident(String),
    VoidPointer,
    Pointer(Box<TypeAtom>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AtomKind {
    NumLit(NumLit),
    StrLit(String),
    CStrLit(CString),
    BoolLit(bool),
    Type(TypeAtom),

    // Ops
    Plus,
    Minus,
    Asterisk,
    Slash,
    Equal,
    Not,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    Percent,

    // Keywords
    Swap,
    Drop,
    Break,
    Load,
    Store,
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
            TokenValue::BoolLit(n) => Ok(Self::BoolLit(n)),
            TokenValue::Plus => Ok(Self::Plus),
            TokenValue::Minus => Ok(Self::Minus),
            TokenValue::Asterisk => Ok(Self::Asterisk),
            TokenValue::Slash => Ok(Self::Slash),
            TokenValue::Equal => Ok(Self::Equal),
            TokenValue::Not => Ok(Self::Not),
            TokenValue::Neq => Ok(Self::Neq),
            TokenValue::Lt => Ok(Self::Lt),
            TokenValue::Lte => Ok(Self::Lte),
            TokenValue::Gt => Ok(Self::Gt),
            TokenValue::Gte => Ok(Self::Gte),
            TokenValue::Percent => Ok(Self::Percent),
            TokenValue::Ellipsis => Err(()),
            TokenValue::Arrow => Err(()),
            TokenValue::Dup => Err(()),
            TokenValue::Swap => Ok(Self::Swap),
            TokenValue::Drop => Ok(Self::Drop),
            TokenValue::Extern => Err(()),
            TokenValue::Fn => Err(()),
            TokenValue::Cast => Err(()),
            TokenValue::Void => Err(()),
            TokenValue::Load => Ok(Self::Load),
            TokenValue::Store => Ok(Self::Store),
            TokenValue::Then => Err(()),
            TokenValue::Else => Err(()),
            TokenValue::While => Err(()),
            TokenValue::Break => Ok(Self::Break),
            TokenValue::PathSep => Err(()),
            TokenValue::Pub => Err(()),
            TokenValue::Mod => Err(()),
            TokenValue::Use => Err(()),
            TokenValue::As => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Atom {
    pub token: Token,
    pub kind: AtomKind,
}

impl Spanned for Atom {
    fn span(&self) -> Span {
        self.token.span()
    }
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
    pub visibility: Visibility,
    pub name: String,
    pub ident_span: Span,
    pub linker_name: String,
    pub args: Vec<TypeAtom>,
    pub variadic: bool,
    pub returns: Vec<TypeAtom>,
}

impl Spanned for ExternFn {
    fn span(&self) -> Span {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fn {
    pub visibility: Visibility,
    pub name: String,
    pub ident_span: Span,
    pub args: Vec<TypeAtom>,
    pub returns: Vec<TypeAtom>,
    pub body: Vec<Ast>,
}

impl Spanned for Fn {
    fn span(&self) -> Span {
        todo!()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Private,
    Public,
}

/// ```stark
/// pub mod foo;
/// mod foo;
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleDeclaration {
    pub name: String,
    pub visibility: Visibility,
    span: Span,
    pub name_span: Span,
}

impl Spanned for ModuleDeclaration {
    fn span(&self) -> Span {
        self.span
    }
}

/// ```stark
/// use foo::bar
/// use foo::bar
/// use foo::bar as baz
/// use foo::(bar baz)
/// use foo::(bar baz as qux)
/// use foo::(bar baz::qux::(quux corge))
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub tree: ImportTree,
    pub visibility: Visibility,
    pub is_root: bool,
    span: Span,
}

impl Spanned for Import {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportTree {
    pub value: ImportTreeValue,
    pub name: PathElement,
    span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportTreeValue {
    Vertex { children: Vec<ImportTree> },
    Leaf { rename: Option<String> },
}

impl Spanned for ImportTree {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseThen {
    pub else_token: Token,
    pub then_token: Token,
    pub condition: Vec<Ast>,
    pub body: Vec<Ast>,
}

impl Spanned for ElseThen {
    fn span(&self) -> Span {
        todo!()
    }
}

/// ```stark
/// ... then { } else .. then { } else { }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Then {
    pub then_token: Token,
    pub body: Vec<Ast>,
    pub body_span: Span,
    pub else_thens: Vec<ElseThen>,
    pub elze: Option<(Vec<Ast>, Token)>,
}

impl Spanned for Then {
    fn span(&self) -> Span {
        todo!()
    }
}

/// ```stark
/// ... while x y z { }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub while_token: Token,
    pub condition: Vec<Ast>,
    pub condition_span: Span,
    pub body: Vec<Ast>,
    pub body_span: Span,
}

impl Spanned for While {
    fn span(&self) -> Span {
        self.while_token.span() + self.body_span
    }
}

/// ```stark
/// ... cast(*u8) ...
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Cast {
    /// ```stark
    /// ... cast(*u8) ...
    ///     ^^^^^^^^^
    /// ```
    span: Span,
    /// ```stark
    /// ... cast(*u8) ...
    ///          ^^^
    /// ```
    pub target_span: Span,
    /// ```stark
    /// ... cast(*u8) ...
    ///          ^^^
    /// ```
    pub target: TypeAtom,
}

impl Spanned for Cast {
    fn span(&self) -> Span {
        self.span
    }
}

/// ```stark
/// ... dup ...
/// ... dup(2) ...
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Dup {
    /// ```stark
    /// ... dup(*u8) ...
    ///     ^^^^^^^^
    /// ```
    span: Span,
    /// ```stark
    /// ... dup(2) ...
    ///         ^
    /// ```
    pub count: u32,
}

impl Spanned for Dup {
    fn span(&self) -> Span {
        self.span
    }
}

// TODO: This isn't really an ast
#[derive(Debug, Clone, PartialEq)]
pub enum Ast {
    Ident(Ident),
    Atom(Atom),
    ExternFn(ExternFn),
    Fn(Fn),
    ModuleDeclaration(ModuleDeclaration),
    Import(Import),
    Then(Then),
    While(While),
    Cast(Cast),
    Dup(Dup),
}

impl Spanned for Ast {
    fn span(&self) -> Span {
        match self {
            Ast::Ident(i) => i.span(),
            Ast::Atom(a) => a.span(),
            Ast::ExternFn(f) => f.span(),
            Ast::Fn(f) => f.span(),
            Ast::ModuleDeclaration(m) => m.span(),
            Ast::Import(i) => i.span(),
            Ast::Then(t) => t.span(),
            Ast::While(w) => w.span(),
            Ast::Cast(c) => c.span(),
            Ast::Dup(d) => d.span(),
        }
    }
}

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
}

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum ParseError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    LexError(#[from] LexError),
    #[error("Unexpected token '{found:?}'")]
    UnexpectedToken {
        #[label = "here"]
        span: Span,
        found: TokenKind,
    },
    #[error("Unexpected token '{found:?}' Expected '{expected:?}'")]
    UnexpectedTokenExpected {
        #[label = "here"]
        span: Span,
        found: TokenKind,
        expected: TokenKind,
    },
    #[error("Unexpected token '{found:?}' Expected one of '{expected:?}'")]
    UnexpectedTokenExpectedMany {
        #[label = "here"]
        span: Span,
        found: TokenKind,
        expected: &'static [TokenKind],
    },
    #[error("Expected {expected}, found EOF")]
    UnexpectedEof {
        expected: String,
        #[label = "while matching this"]
        matching: Option<Span>,
    },
    #[error("Expected integer, found float value {found}")]
    ExpectedInteger {
        #[label = "here"]
        span: Span,
        found: f64,
    },
    #[error("Integer out of range.  Got: {found}, expected integer in [{min}, {max}]")]
    IntegerOutOfBounds {
        #[label = "here"]
        span: Span,
        found: i128,
        min: i128,
        max: i128,
    },
    #[error("Nested function definitions are not supported")]
    NestedFunction {
        #[label = "Function definition"]
        span: Span,
    },
}

impl ParseError {
    pub fn unexpected_token(token: Token, expected: &'static [TokenKind]) -> Self {
        match expected {
            [] => Self::UnexpectedToken {
                span: token.span(),
                found: token.kind(),
            },
            [expected] => Self::UnexpectedTokenExpected {
                span: token.span(),
                found: token.kind(),
                expected: *expected,
            },
            _ => Self::UnexpectedTokenExpectedMany {
                span: token.span(),
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

    fn peek_token(&'a mut self) -> Result<Option<&'a Token>, ParseError> {
        match self.tokens.peek().map(Result::as_ref).transpose() {
            Ok(t) => Ok(t),
            Err(e) => Err(e.clone().into()),
        }
    }

    fn take_type(&mut self) -> Result<(TypeAtom, Span), ParseError> {
        let Some(token) = self.tokens.next() else {
            return Err(ParseError::UnexpectedEof {
                expected: "type".into(),
                matching: None,
            });
        };

        let token = token?;
        let span = token.span();
        match token.value {
            TokenValue::Asterisk => {
                if let Some(Ok(t)) = self.tokens.peek()
                    && t.kind() == TokenKind::Void
                {
                    let t = self
                        .tokens
                        .next()
                        .expect("peek is some")
                        .expect("peek is ok");
                    Ok((TypeAtom::VoidPointer, token.span() + t.span()))
                } else {
                    let (inner, span) = self.take_type()?;
                    let span = token.span() + span;
                    Ok((TypeAtom::Pointer(Box::new(inner)), span))
                }
            }
            TokenValue::Ident(ident) => Ok((TypeAtom::Ident(ident), span)),
            _ => Err(ParseError::unexpected_token(
                token,
                &[TokenKind::Ident, TokenKind::Asterisk],
            )),
        }
    }

    fn take_args(&mut self, allow_variadic: bool) -> Result<(Vec<TypeAtom>, bool), ParseError> {
        let mut args = Vec::new();
        let mut variadic = false;
        while let Some(t) = self.tokens.peek() {
            if t.is_err() {
                let t = self.tokens.next().expect("peek is some");
                return Err(t.expect_err("t.is_err() called above").into());
            }
            let t = t.as_ref().unwrap();
            match t.value {
                TokenValue::Ident(_) if !variadic => {
                    let (ty, _) = self.take_type()?;
                    args.push(ty);
                }
                TokenValue::Asterisk => {
                    let (ty, _) = self.take_type()?;
                    args.push(ty);
                }
                TokenValue::RParen => {
                    let _ = self.tokens.next().unwrap()?;
                    break;
                }
                TokenValue::Ellipsis if !variadic => {
                    let t = self.tokens.next().unwrap()?;
                    if !allow_variadic {
                        return Err(ParseError::unexpected_token(
                            t,
                            &[TokenKind::Ident, TokenKind::RParen],
                        ));
                    }
                    variadic = true;
                }
                _ => {
                    let t = self.tokens.next().unwrap()?;
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

    fn take_extern_fn(&mut self, vis: Visibility) -> Result<ExternFn, ParseError> {
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
            visibility: vis,
            name: ident.to_string(),
            ident_span: ident_token.span(),
            linker_name: ident,
            args,
            variadic,
            returns,
        })
    }

    fn take_path(&mut self, taken: Token) -> Result<Path, ParseError> {
        let mut span = taken.span();
        let mut path: Vec<PathElement> = Vec::new();
        let is_root = match taken.value {
            TokenValue::Ident(name) => {
                path.push(PathElement { name, span });
                false
            }
            TokenValue::PathSep => {
                let (ident, name) = expect_token!(self, Ident(_));
                path.push(PathElement {
                    name,
                    span: ident.span(),
                });
                span += ident.span();
                true
            }
            _ => {
                // unreachable is probably more accurate, but just in case, but that's okay
                return Err(ParseError::unexpected_token(
                    taken,
                    &[TokenKind::Ident, TokenKind::PathSep],
                ));
            }
        };

        loop {
            if try_expect_token!(self, PathSep).is_none() {
                break;
            }
            let (ident, name) = expect_token!(self, Ident(_));
            path.push(PathElement {
                name,
                span: ident.span(),
            });
            span += ident.span();
        }

        Ok(Path {
            inner: path,
            is_root,
            span,
        })
    }

    fn take_arity(&mut self) -> Result<Option<(u32, Span)>, ParseError> {
        let Some(lparen) = try_expect_token!(self, LParen) else {
            return Ok(None);
        };
        let (num, n) = expect_token!(self, NumLit(_));
        let (rparen, _) = expect_token!(self, RParen);

        let n = match n.value {
            NumLitVal::Integer(n) => {
                if !(1..=u32::MAX as _).contains(&n) {
                    return Err(ParseError::IntegerOutOfBounds {
                        span: num.span(),
                        found: n,
                        min: 1,
                        max: u32::MAX.into(),
                    });
                }
                n
            }
            NumLitVal::Float(val) => {
                return Err(ParseError::ExpectedInteger {
                    span: num.span(),
                    found: val,
                });
            }
        };

        Ok(Some((n as u32, lparen.span() + rparen.span())))
    }

    fn take_ident(&mut self, taken: Token) -> Result<Ident, ParseError> {
        let path = self.take_path(taken)?;

        let (arity, span) = if let Some((arity, span)) = self.take_arity()? {
            (Some(arity), path.span() + span)
        } else {
            (None, path.span())
        };

        Ok(Ident { path, arity, span })
    }

    fn take_import_tree(&mut self) -> Result<ImportTree, ParseError> {
        let (token, name) = expect_token!(self, Ident(_));
        let name = PathElement {
            name,
            span: token.span(),
        };

        if try_expect_token!(self, PathSep).is_none() {
            let (span, rename) = if try_expect_token!(self, As).is_some() {
                let (rn_token, rename) = expect_token!(self, Ident(_));
                (token.span() + rn_token.span(), Some(rename))
            } else {
                (token.span(), None)
            };
            return Ok(ImportTree {
                span,
                name,
                value: ImportTreeValue::Leaf { rename },
            });
        }

        let (span, value) = if try_expect_token!(self, LParen).is_some() {
            let mut children = Vec::new();
            let mut span = token.span();
            loop {
                let tree = self.take_import_tree()?;
                span += tree.span();
                children.push(tree);
                if try_expect_token!(self, RParen).is_some() {
                    break;
                }
            }
            (span, ImportTreeValue::Vertex { children })
        } else {
            let tree = self.take_import_tree()?;
            (
                token.span() + tree.span(),
                ImportTreeValue::Vertex {
                    children: vec![tree],
                },
            )
        };

        Ok(ImportTree { span, value, name })
    }

    fn take_fn(&mut self, vis: Visibility) -> Result<Fn, ParseError> {
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

        let (body, _) = self.parse_body(TokenKind::RCurly, Some(&body_open))?;

        Ok(Fn {
            visibility: vis,
            name: ident,
            ident_span: ident_token.span(),
            args,
            returns,
            body,
        })
    }

    fn take_cast(&mut self, cast_token: Token) -> Result<Cast, ParseError> {
        let (group_start, _) = expect_token!(self, LParen);
        let (target, target_span) = self.take_type()?;
        let (group_end, _) = expect_token!(self, RParen);

        Ok(Cast {
            span: cast_token.span() + group_start.span() + group_end.span(),
            target_span,
            target,
        })
    }

    fn take_then(&mut self, then_token: Token) -> Result<Then, ParseError> {
        // expect_token!(self, Then);
        let (body_start, _) = expect_token!(self, LCurly);
        let (body, body_end) = self.parse_body(TokenKind::RCurly, Some(&then_token))?;
        let body_span = body_start.span() + body_end.span();

        let mut else_thens = Vec::new();

        loop {
            let Some(else_token) = try_expect_token!(self, Else) else {
                return Ok(Then {
                    then_token,
                    body,
                    body_span,
                    else_thens,
                    elze: None,
                });
            };

            match try_expect_token!(self, LCurly) {
                Some(l) => {
                    let (else_body, _) = self.parse_body(TokenKind::RCurly, Some(&l))?;
                    return Ok(Then {
                        then_token,
                        body,
                        body_span,
                        else_thens,
                        elze: Some((else_body, else_token)),
                    });
                }
                None => {
                    let (condition, then_token) = self.parse_body(TokenKind::Then, None)?;
                    let (l, ()) = expect_token!(self, LCurly);
                    let (body, _) = self.parse_body(TokenKind::RCurly, Some(&l))?;
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
        let (condition, l_curly) = self.parse_body(TokenKind::LCurly, Some(&while_token))?;
        let (body, r_curly) = self.parse_body(TokenKind::RCurly, Some(&l_curly))?;

        let c_start = condition[0].span();
        let c_end = condition[condition.len() - 1].span();

        let body_span = l_curly.span() + r_curly.span();

        Ok(While {
            while_token,
            condition,
            condition_span: c_start + c_end,
            body,
            body_span,
        })
    }

    fn parse_body(
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
                TokenValue::PathSep | TokenValue::Ident(_) => {
                    let ident = self.take_ident(token)?;
                    out.push(Ast::Ident(ident));
                }
                TokenValue::LParen => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::RParen => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::LCurly => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::RCurly => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::Semicolon => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::StrLit(_)
                | TokenValue::CStrLit(_)
                | TokenValue::NumLit(_)
                | TokenValue::BoolLit(_)
                | TokenValue::Plus
                | TokenValue::Minus
                | TokenValue::Asterisk
                | TokenValue::Slash
                | TokenValue::Equal
                | TokenValue::Not
                | TokenValue::Neq
                | TokenValue::Lt
                | TokenValue::Lte
                | TokenValue::Gt
                | TokenValue::Gte
                | TokenValue::Percent
                | TokenValue::Swap
                | TokenValue::Drop
                | TokenValue::Load
                | TokenValue::Store
                | TokenValue::Break => out.push(Ast::Atom(token.try_into()?)),
                TokenValue::Ellipsis => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::Arrow => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::Extern => Err(ParseError::NestedFunction { span: token.span() })?,
                TokenValue::Fn => Err(ParseError::NestedFunction { span: token.span() })?,
                TokenValue::Cast => {
                    out.push(Ast::Cast(self.take_cast(token)?));
                }
                TokenValue::Dup => {
                    let (count, span) = if let Some((arity, span)) = self.take_arity()? {
                        (arity, token.span() + span)
                    } else {
                        (1, token.span())
                    };

                    out.push(Ast::Dup(Dup { span, count }));
                }
                TokenValue::Void => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::Then => {
                    let t = self.take_then(token)?;
                    out.push(Ast::Then(t));
                }
                TokenValue::Else => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::While => {
                    out.push(Ast::While(self.take_while(token)?));
                }
                TokenValue::Pub => todo!(),
                TokenValue::Mod => todo!(),
                TokenValue::Use => todo!(),
                TokenValue::As => todo!(),
            }
        }

        Err(ParseError::UnexpectedEof {
            expected: format!("{:?}", end_token),
            matching: open_token.map(|t| t.span()),
        })
    }

    pub fn parse_module(mut self) -> Result<Vec<Ast>, ParseError> {
        let mut out = Vec::new();

        while let Some(token) = self.tokens.next() {
            let token = token?;
            match token.value {
                TokenValue::PathSep
                | TokenValue::Ident(_)
                | TokenValue::LParen
                | TokenValue::RParen
                | TokenValue::LCurly
                | TokenValue::RCurly
                | TokenValue::Semicolon
                | TokenValue::StrLit(_)
                | TokenValue::CStrLit(_)
                | TokenValue::NumLit(_)
                | TokenValue::BoolLit(_)
                | TokenValue::Plus
                | TokenValue::Minus
                | TokenValue::Asterisk
                | TokenValue::Slash
                | TokenValue::Not
                | TokenValue::Neq
                | TokenValue::Lt
                | TokenValue::Lte
                | TokenValue::Gt
                | TokenValue::Gte
                | TokenValue::Percent
                | TokenValue::Equal
                | TokenValue::Swap
                | TokenValue::Drop
                | TokenValue::Load
                | TokenValue::Store
                | TokenValue::Break
                | TokenValue::Dup
                | TokenValue::Ellipsis
                | TokenValue::Arrow
                | TokenValue::Then
                | TokenValue::Else
                | TokenValue::While
                | TokenValue::Cast
                | TokenValue::Void => Err(ParseError::unexpected_token(token, &[]))?,
                TokenValue::Extern => {
                    let f = self.take_extern_fn(Visibility::Private)?;
                    out.push(Ast::ExternFn(f));
                }
                TokenValue::Fn => {
                    let f = self.take_fn(Visibility::Private)?;
                    out.push(Ast::Fn(f));
                }
                TokenValue::Pub => 'a: {
                    if try_expect_token!(self, Extern).is_some() {
                        let f = self.take_extern_fn(Visibility::Public)?;
                        out.push(Ast::ExternFn(f));
                        break 'a;
                    }

                    if try_expect_token!(self, Fn).is_some() {
                        let f = self.take_fn(Visibility::Public)?;
                        out.push(Ast::Fn(f));
                        break 'a;
                    }

                    if try_expect_token!(self, Use).is_some() {
                        todo!("pub use")
                    }

                    if try_expect_token!(self, Mod).is_some() {
                        let (ident, name) = expect_token!(self, Ident(_));
                        out.push(Ast::ModuleDeclaration(ModuleDeclaration {
                            name,
                            visibility: Visibility::Public,
                            span: token.span() + ident.span(),
                            name_span: ident.span(),
                        }));
                        break 'a;
                    }

                    Err(ParseError::unexpected_token(
                        token,
                        &[
                            TokenKind::Fn,
                            TokenKind::Use,
                            TokenKind::Mod,
                            TokenKind::Extern,
                        ],
                    ))?
                }
                TokenValue::Mod => {
                    let (ident, name) = expect_token!(self, Ident(_));
                    out.push(Ast::ModuleDeclaration(ModuleDeclaration {
                        name,
                        visibility: Visibility::Private,
                        span: token.span() + ident.span(),
                        name_span: ident.span(),
                    }));
                }
                TokenValue::Use => {
                    let is_root = try_expect_token!(self, PathSep).is_some();
                    let tree = self.take_import_tree()?;
                    out.push(Ast::Import(Import {
                        span: token.span() + tree.span(),
                        visibility: Visibility::Private,
                        tree,
                        is_root,
                    }));
                }
                TokenValue::As => Err(ParseError::unexpected_token(token, &[]))?,
            }
        }

        Ok(out)
    }
}
