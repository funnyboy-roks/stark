use std::{
    collections::{HashMap, VecDeque},
    ffi::CString,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
};

use miette::Diagnostic;
use phf::phf_map;
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
            if i != 0 || self.is_root {
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

macro_rules! define_atom_kind {
    ($( $($t: literal =>)? $ident: ident$(($ty: ty))? ),+$(,)?) => {
        #[derive(Debug, Clone, PartialEq)]
        pub enum AtomKind {
            $(
                $ident$(($ty))?
            ),+
        }

        impl TryFrom<TokenValue> for AtomKind {
            type Error = ();

            fn try_from(value: TokenValue) -> Result<Self, Self::Error> {
                $(
                    define_atom_kind!(@ value $($t =>)? $ident$(($ty))?);
                )+
                Err(())
            }
        }
    };
    (@ $value: ident $t: literal => $ident: ident$(($ty: ty))?) => {
        if let TokenValue::$ident$((define_atom_kind!(@_ $ty, x)))? = $value {
            return Ok(Self::$ident$((define_atom_kind!(@_ $ty, x)))?);
        }
    };
    (@_ $ty: ty, $ident: ident) => {
        $ident
    };
    (@ $value: ident $ident: ident$(($ty: ty))?) => {
    };
}

// `0 =>` indicates that this kind is derived directly from a token
define_atom_kind! {
    0 => NumLit(NumLit),
    0 => StrLit(String),
    0 => CStrLit(CString),
    0 => BoolLit(bool),
    Type(TypeAtom),

    // Ops
    0 => Plus,
    0 => Minus,
    0 => Asterisk,
    0 => Slash,
    0 => Equal,
    0 => Not,
    0 => Neq,
    0 => Lt,
    0 => Lte,
    0 => Gt,
    0 => Gte,
    0 => Percent,
    0 => Ampersand,
    0 => Pipe,
    0 => Caret,
    0 => Tilde,
    0 => Shl,
    0 => Shr,

    // Keywords
    0 => Swap,
    0 => Drop,
    0 => Break,
    0 => Load,
    0 => Store,
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
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        let kind = token.value.clone().try_into()?;
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
    span: Span,
}

impl Spanned for ExternFn {
    fn span(&self) -> Span {
        self.span
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
    span: Span,
}

impl Spanned for Fn {
    fn span(&self) -> Span {
        self.span
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

#[derive(Debug, Clone, Copy)]
pub struct DisplayList<'a, D: Display>(&'a [D]);
impl<D: Display> Display for DisplayList<'_, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            [] => write!(f, "token"),
            [a] => write!(f, "{a}"),
            [a, b] => write!(f, "one of {a} or {b}"),
            expected => {
                for (i, e) in expected.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{e}")?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum MacroArgKind {
    Ident,
    Token,
    Literal,
}

const MACRO_ARG_KIND: phf::Map<&str, MacroArgKind> = phf_map! {
    "ident" => MacroArgKind::Ident,
    "token" => MacroArgKind::Token,
    "literal" => MacroArgKind::Literal,
};

#[derive(Clone, Debug, PartialEq, Eq)]
struct MacroArg {
    name_span: Span,
    name: String,
    kind: MacroArgKind,
}

#[derive(Clone, Debug)]
struct Macro {
    macro_span: Span,
    ident_span: Span,
    body_span: Span,
    tokens: Vec<Token>,
    args: Option<Vec<MacroArg>>,
}

pub struct Parser<'a> {
    len: usize,
    lexer: Lexer<'a>,
    queued_tokens: VecDeque<Token>,
    macros: HashMap<String, Macro>,
}

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum ParseError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    LexError(#[from] LexError),
    #[error("Unexpected token {found}")]
    UnexpectedToken {
        #[label = "here"]
        span: Span,
        found: TokenKind,
    },
    #[error("Expected {expected}, found {found}")]
    UnexpectedTokenExpected {
        #[label = "here"]
        span: Span,
        found: TokenKind,
        expected: DisplayList<'static, TokenKind>,
    },
    #[error("Expected {expected}, found EOF")]
    UnexpectedEof {
        #[label("Expected {expected}, found EOF")]
        span: Span,
        expected: DisplayList<'static, TokenKind>,
        #[label = "while matching this"]
        matching: Option<Span>,
    },
    #[error("Expected integer, found float value {found:?}")]
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
    #[error("Invalid macro argument kind: '{kind}'")]
    #[diagnostic(help("Use one of: {:?}", MACRO_ARG_KIND.keys().collect::<Vec<_>>()))]
    InvalidMacroArgKind {
        #[label = "this argument"]
        arg: Span,
        kind: String,
        #[label = "Invalid macro argument kind"]
        kind_span: Span,
    },
    #[error("Undefined macro: #{name}")]
    UndefinedMacro {
        name: String,
        #[label = "Used here"]
        span: Span,
    },
    #[error("Expected literal, got {}", kind)]
    ExpectedLiteral {
        #[label = "this argument"]
        span: Span,
        #[label = "in this macro invocation"]
        makro: Span,
        kind: TokenKind,
    },
    #[error("Undefined macro variable ${}", var)]
    UndefinedMacroVariable {
        var: String,
        #[label = "here"]
        span: Span,
        #[label = "in this macro definition"]
        makro: Span,
    },
}

impl ParseError {
    pub fn unexpected_eof(
        span: Span,
        expected: &'static [TokenKind],
        matching: Option<&Token>,
    ) -> Self {
        Self::UnexpectedEof {
            span,
            expected: DisplayList(expected),
            matching: matching.map(|t| t.span()),
        }
    }
    pub fn unexpected_token(token: Token, expected: &'static [TokenKind]) -> Self {
        if expected.is_empty() {
            Self::UnexpectedToken {
                span: token.span(),
                found: token.kind(),
            }
        } else {
            Self::UnexpectedTokenExpected {
                span: token.span(),
                found: token.kind(),
                expected: DisplayList(expected),
            }
        }
    }
}

macro_rules! expect_token {
    ($self: ident, $ident: ident) => {{
        let Some(token) = $self.take_token().map_err(|e| vec![e.into()])? else {
            return Err(vec![ParseError::unexpected_eof(
                $self.end_span(),
                &[TokenKind::$ident],
                None,
            )]);
        };

        match token.value {
            TokenValue::$ident => (token, ()),
            _ => {
                return Err(vec![ParseError::unexpected_token(
                    token,
                    &[TokenKind::$ident],
                )]);
            }
        }
    }};
    ($self: ident, $ident: ident(_)) => {{
        let Some(token) = $self.take_token().map_err(|e| vec![e.into()])? else {
            return Err(vec![ParseError::unexpected_eof(
                $self.end_span(),
                &[TokenKind::$ident],
                None,
            )]);
        };

        match token.value {
            TokenValue::$ident(ref x) => {
                let x = x.clone();
                (token, x)
            }
            _ => {
                return Err(vec![ParseError::unexpected_token(
                    token,
                    &[TokenKind::$ident],
                )]);
            }
        }
    }};
}

macro_rules! try_expect_token {
    ($self: ident, $ident: ident) => {
        'a: {
            let Some(token) = $self.peek_token().map_err(|e| vec![e.into()])? else {
                break 'a None;
            };

            match token.kind() {
                TokenKind::$ident => {
                    let token = $self
                        .take_token()
                        .expect("checked above")
                        .expect("checked above");
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
            len: lexer.content.len(),
            lexer,
            queued_tokens: Default::default(),
            macros: Default::default(),
        }
    }

    fn peek_token(&mut self) -> Result<Option<&Token>, LexError> {
        if self.queued_tokens.is_empty() {
            let Some(tok) = self.lexer.next().transpose()? else {
                return Ok(None);
            };
            self.queued_tokens.push_back(tok);
        }
        Ok(self.queued_tokens.front())
    }

    fn take_token(&mut self) -> Result<Option<Token>, LexError> {
        if self.queued_tokens.is_empty() {
            let Some(tok) = self.lexer.next().transpose()? else {
                return Ok(None);
            };
            self.queued_tokens.push_back(tok);
        }
        Ok(self.queued_tokens.pop_front())
    }

    fn end_span(&self) -> Span {
        Span::new(self.len.saturating_sub(1)..self.len)
    }

    fn take_type(&mut self) -> Result<(TypeAtom, Span), Vec<ParseError>> {
        let Some(token) = self.take_token().map_err(|e| vec![e.into()])? else {
            return Err(vec![ParseError::unexpected_eof(
                self.end_span(),
                &[TokenKind::Asterisk, TokenKind::Ident],
                None,
            )]);
        };

        let span = token.span();
        match token.value {
            TokenValue::Asterisk => {
                if let Ok(Some(t)) = self.peek_token()
                    && t.kind() == TokenKind::Void
                {
                    let t = self
                        .take_token()
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
            _ => Err(vec![ParseError::unexpected_token(
                token,
                &[TokenKind::Ident, TokenKind::Asterisk],
            )]),
        }
    }

    fn take_args(
        &mut self,
        allow_variadic: bool,
    ) -> Result<(Vec<TypeAtom>, bool, Span), Vec<ParseError>> {
        let mut args = Vec::new();
        let mut variadic = false;
        let mut span = Span::none();
        while let Some(t) = self.peek_token().map_err(|e| vec![e.into()])? {
            span += t.span();
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
                    let _ = self
                        .take_token()
                        .expect("checked above")
                        .expect("checked above");
                    break;
                }
                TokenValue::Ellipsis if !variadic => {
                    let t = self
                        .take_token()
                        .expect("checked above")
                        .expect("checked above");
                    if !allow_variadic {
                        return Err(vec![ParseError::unexpected_token(
                            t,
                            &[TokenKind::Ident, TokenKind::RParen],
                        )]);
                    }
                    variadic = true;
                }
                _ => {
                    let t = self
                        .take_token()
                        .expect("checked above")
                        .expect("checked above");
                    return Err(vec![ParseError::unexpected_token(
                        t,
                        if variadic {
                            &[TokenKind::RParen]
                        } else if allow_variadic {
                            &[TokenKind::Ident, TokenKind::RParen, TokenKind::Ellipsis]
                        } else {
                            &[TokenKind::Ident, TokenKind::RParen]
                        },
                    )]);
                }
            }
        }
        Ok((args, variadic, span))
    }

    fn take_extern_fn(
        &mut self,
        vis: Visibility,
        extern_span: Span,
    ) -> Result<ExternFn, Vec<ParseError>> {
        let mut span = extern_span;
        let linker_symbol = if try_expect_token!(self, LParen).is_some() {
            let (_, s) = expect_token!(self, StrLit(_));
            expect_token!(self, RParen);
            Some(s)
        } else {
            None
        };
        // expect_token!(self, Extern);
        expect_token!(self, Fn);
        let (ident_token, ident) = expect_token!(self, Ident(_));
        span += ident_token.span();

        let Some(next) = self.peek_token().map_err(|e| vec![e.into()])? else {
            return Err(vec![ParseError::unexpected_eof(
                self.end_span(),
                &[TokenKind::LParen, TokenKind::Arrow, TokenKind::Semicolon],
                None,
            )]);
        };
        span += next.span();
        let (args, variadic, args_span) = match next.kind() {
            TokenKind::LParen => {
                expect_token!(self, LParen);
                self.take_args(true)?
            }
            TokenKind::Arrow => (Vec::new(), false, next.span()),
            TokenKind::Semicolon => (Vec::new(), false, next.span()),
            _ => {
                return Err(vec![ParseError::unexpected_token(
                    self.take_token()
                        .expect("checked above")
                        .expect("checked above"),
                    &[TokenKind::LParen, TokenKind::Arrow, TokenKind::Semicolon],
                )]);
            }
        };
        span += args_span;

        let Some(next) = self.take_token().map_err(|e| vec![e.into()])? else {
            return Err(vec![ParseError::unexpected_eof(
                self.end_span(),
                &[TokenKind::Semicolon, TokenKind::Arrow],
                None,
            )]);
        };
        span += next.span();
        let returns = match next.kind() {
            TokenKind::Semicolon => Vec::new(),
            TokenKind::Arrow => {
                expect_token!(self, LParen);
                let (returns, _, ret_span) = self.take_args(false)?;
                span += ret_span;
                expect_token!(self, Semicolon);
                returns
            }
            _ => {
                return Err(vec![ParseError::unexpected_token(
                    next,
                    &[TokenKind::LCurly, TokenKind::Arrow],
                )]);
            }
        };

        Ok(ExternFn {
            visibility: vis,
            name: ident.to_string(),
            ident_span: ident_token.span(),
            linker_name: linker_symbol.unwrap_or(ident),
            args,
            variadic,
            returns,
            span,
        })
    }

    fn take_path(&mut self, taken: Token) -> Result<Path, Vec<ParseError>> {
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
                // unreachable is probably more accurate, but let's error just in case
                return Err(vec![ParseError::unexpected_token(
                    taken,
                    &[TokenKind::Ident, TokenKind::PathSep],
                )]);
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

    fn take_arity(&mut self) -> Result<Option<(u32, Span)>, Vec<ParseError>> {
        let Some(lparen) = try_expect_token!(self, LParen) else {
            return Ok(None);
        };
        let (num, n) = expect_token!(self, NumLit(_));
        let (rparen, _) = expect_token!(self, RParen);

        let n = match n.value {
            NumLitVal::Integer(n) => {
                if !(1..=u32::MAX as _).contains(&n) {
                    return Err(vec![ParseError::IntegerOutOfBounds {
                        span: num.span(),
                        found: n,
                        min: 1,
                        max: u32::MAX.into(),
                    }]);
                }
                n
            }
            NumLitVal::Float(val) => {
                return Err(vec![ParseError::ExpectedInteger {
                    span: num.span(),
                    found: val,
                }]);
            }
        };

        Ok(Some((n as u32, lparen.span() + rparen.span())))
    }

    fn take_ident(&mut self, taken: Token) -> Result<Ident, Vec<ParseError>> {
        let path = self.take_path(taken)?;

        let (arity, span) = if let Some((arity, span)) = self.take_arity()? {
            (Some(arity), path.span() + span)
        } else {
            (None, path.span())
        };

        Ok(Ident { path, arity, span })
    }

    fn take_import_tree(&mut self) -> Result<ImportTree, Vec<ParseError>> {
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

    fn take_fn(&mut self, vis: Visibility, fn_span: Span) -> Result<Fn, Vec<ParseError>> {
        // expect_token!(self, Fn);
        let mut span = fn_span;
        let (ident_token, ident) = expect_token!(self, Ident(_));
        span += ident_token.span();

        let Some(next) = self.peek_token().map_err(|e| vec![e.into()])? else {
            return Err(vec![ParseError::unexpected_eof(
                self.end_span(),
                &[TokenKind::LParen, TokenKind::Arrow, TokenKind::LCurly],
                None,
            )]);
        };
        span += next.span();
        let (args, _, args_span) = match next.kind() {
            TokenKind::LParen => {
                expect_token!(self, LParen);
                // TODO: allow variadic?
                self.take_args(false)?
            }
            TokenKind::Arrow => (Vec::new(), false, next.span()),
            TokenKind::LCurly => (Vec::new(), false, next.span()),
            _ => {
                return Err(vec![ParseError::unexpected_token(
                    self.take_token()
                        .expect("checked above")
                        .expect("checked above"),
                    &[TokenKind::LCurly, TokenKind::Arrow],
                )]);
            }
        };
        span += args_span;

        let Some(next) = self.take_token().map_err(|e| vec![e.into()])? else {
            return Err(vec![ParseError::unexpected_eof(
                self.end_span(),
                &[TokenKind::LCurly, TokenKind::Arrow],
                None,
            )]);
        };
        span += next.span();
        let (returns, body_open) = match next.kind() {
            TokenKind::LCurly => (Vec::new(), next),
            TokenKind::Arrow => {
                expect_token!(self, LParen);
                let (returns, _, ret_span) = self.take_args(false)?;
                span += ret_span;
                let (body_open, ()) = expect_token!(self, LCurly);
                (returns, body_open)
            }
            _ => {
                return Err(vec![ParseError::unexpected_token(
                    next,
                    &[TokenKind::LCurly, TokenKind::Arrow],
                )]);
            }
        };

        let (body, r_curly) = self.parse_body(TokenKind::RCurly, Some(&body_open))?;
        span += r_curly.span();

        Ok(Fn {
            visibility: vis,
            name: ident,
            ident_span: ident_token.span(),
            args,
            returns,
            body,
            span,
        })
    }

    fn take_cast(&mut self, cast_token: Token) -> Result<Cast, Vec<ParseError>> {
        let (group_start, _) = expect_token!(self, LParen);
        let (target, target_span) = self.take_type()?;
        let (group_end, _) = expect_token!(self, RParen);

        Ok(Cast {
            span: cast_token.span() + group_start.span() + group_end.span(),
            target_span,
            target,
        })
    }

    fn take_then(&mut self, then_token: Token) -> Result<Then, Vec<ParseError>> {
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

    fn take_while(&mut self, while_token: Token) -> Result<While, Vec<ParseError>> {
        // expect_token!(self, While);
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

    fn expand_macro(&mut self, m: Macro, span: Span) -> Result<(), Vec<ParseError>> {
        let mut errors = Vec::new();
        let mut arg_values = HashMap::new();
        if let Some(args) = m.args {
            expect_token!(self, LParen);
            'arg_loop: for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    expect_token!(self, Comma);
                }
                let tok = match arg.kind {
                    MacroArgKind::Ident => expect_token!(self, Ident(_)).0,
                    MacroArgKind::Token => {
                        let Some(token) = self.take_token().map_err(|e| vec![e.into()])? else {
                            return Err(vec![ParseError::unexpected_eof(
                                self.end_span(),
                                &[],
                                None,
                            )]);
                        };
                        token
                    }
                    MacroArgKind::Literal => 'x: {
                        if let Some(t) = try_expect_token!(self, NumLit) {
                            break 'x t;
                        };
                        if let Some(t) = try_expect_token!(self, StrLit) {
                            break 'x t;
                        };
                        if let Some(t) = try_expect_token!(self, CStrLit) {
                            break 'x t;
                        };
                        if let Some(t) = try_expect_token!(self, BoolLit) {
                            break 'x t;
                        };
                        let token = self.take_token().unwrap().unwrap();
                        errors.push(ParseError::ExpectedLiteral {
                            span: token.span(),
                            makro: span,
                            kind: token.kind(),
                        });
                        continue 'arg_loop;
                    }
                };
                arg_values.insert(arg.name.clone(), tok);
            }
            expect_token!(self, RParen);
        }
        self.queued_tokens.reserve(m.tokens.len());
        for t in m.tokens.iter().rev().cloned() {
            let span = t.span();
            let t = match t.value {
                TokenValue::MacroVar(v) => {
                    if let Some(t) = arg_values.get(&v) {
                        t.clone()
                    } else {
                        errors.push(ParseError::UndefinedMacroVariable {
                            var: v,
                            span,
                            makro: m.ident_span,
                        });
                        continue;
                    }
                }
                _ => t,
            };
            self.queued_tokens.push_front(t);
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn parse_body(
        &mut self,
        end_token: TokenKind,
        open_token: Option<&Token>,
    ) -> Result<(Vec<Ast>, Token), Vec<ParseError>> {
        let mut out = Vec::new();
        let mut errors = Vec::new();

        loop {
            let token = match self.take_token() {
                Ok(Some(token)) => token,
                Ok(None) => break,
                Err(e) => {
                    errors.push(e.into());
                    continue;
                }
            };

            let x = || {
                let token = token;
                if token.kind() == end_token {
                    return Ok(Some((std::mem::take(&mut out), token)));
                }
                let span = token.span();
                match token.value {
                    TokenValue::PathSep | TokenValue::Ident(_) => {
                        let ident = self.take_ident(token)?;
                        out.push(Ast::Ident(ident));
                    }
                    TokenValue::MacroIdent(ident) => {
                        if let Some(m) = self.macros.get(&ident) {
                            self.expand_macro(m.clone(), span)?;
                        } else {
                            // consume args, as that's probably what the user wants
                            let mut span = span;
                            if try_expect_token!(self, LParen).is_some() {
                                loop {
                                    if let Some(rparen) = try_expect_token!(self, RParen) {
                                        span += rparen.span();
                                        break;
                                    }
                                    let t = self.take_token().unwrap().unwrap();
                                    span += t.span();
                                }
                            }
                            return Err(vec![ParseError::UndefinedMacro { name: ident, span }]);
                        }
                    }
                    TokenValue::MacroVar(_)
                    | TokenValue::Comma
                    | TokenValue::Colon
                    | TokenValue::LParen
                    | TokenValue::RParen
                    | TokenValue::LCurly
                    | TokenValue::RCurly
                    | TokenValue::Ellipsis
                    | TokenValue::Arrow
                    | TokenValue::Void
                    | TokenValue::Else
                    | TokenValue::Semicolon
                    | TokenValue::Pub
                    | TokenValue::Mod
                    | TokenValue::Use
                    | TokenValue::As
                    | TokenValue::Macro => Err(vec![ParseError::unexpected_token(token, &[])])?,
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
                    | TokenValue::Ampersand
                    | TokenValue::Pipe
                    | TokenValue::Caret
                    | TokenValue::Tilde
                    | TokenValue::Shl
                    | TokenValue::Shr
                    | TokenValue::Swap
                    | TokenValue::Drop
                    | TokenValue::Load
                    | TokenValue::Store
                    | TokenValue::Break => out.push(Ast::Atom(
                        Atom::try_from(token).expect("all of these types have try_from"),
                    )),
                    TokenValue::Extern => {
                        let f = self.take_extern_fn(Visibility::Private, token.span())?;
                        Err(vec![ParseError::NestedFunction { span: f.span() }])?
                    }
                    TokenValue::Fn => {
                        let f = self.take_fn(Visibility::Private, token.span())?;
                        Err(vec![ParseError::NestedFunction { span: f.span() }])?
                    }
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
                    TokenValue::Then => {
                        let t = self.take_then(token)?;
                        out.push(Ast::Then(t));
                    }
                    TokenValue::While => {
                        out.push(Ast::While(self.take_while(token)?));
                    }
                }
                Result::<_, Vec<ParseError>>::Ok(None)
            };
            match x() {
                Ok(Some(v)) if errors.is_empty() => return Ok(v),
                Ok(Some(_)) => return Err(errors),
                Ok(_) => {}
                Err(e) => errors.extend(e),
            }
        }

        errors.push(ParseError::unexpected_eof(self.end_span(), &[], open_token));
        Err(errors)
    }

    pub fn parse_module(mut self) -> Result<Vec<Ast>, Vec<ParseError>> {
        let mut out = Vec::new();

        let mut errors = Vec::new();

        loop {
            let token = match self.take_token() {
                Ok(Some(token)) => token,
                Ok(None) => break,
                Err(e) => {
                    errors.push(e.into());
                    continue;
                }
            };

            let span = token.span();
            let x = || {
                match token.value {
                    TokenValue::PathSep
                    | TokenValue::Ident(_)
                    | TokenValue::MacroVar(_)
                    | TokenValue::LParen
                    | TokenValue::RParen
                    | TokenValue::LCurly
                    | TokenValue::RCurly
                    | TokenValue::Semicolon
                    | TokenValue::StrLit(_)
                    | TokenValue::CStrLit(_)
                    | TokenValue::NumLit(_)
                    | TokenValue::BoolLit(_)
                    | TokenValue::Comma
                    | TokenValue::Colon
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
                    | TokenValue::Ampersand
                    | TokenValue::Pipe
                    | TokenValue::Caret
                    | TokenValue::Tilde
                    | TokenValue::Shl
                    | TokenValue::Shr
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
                    | TokenValue::Void => Err(vec![ParseError::unexpected_token(token, &[])])?,
                    TokenValue::Extern => {
                        let f = self.take_extern_fn(Visibility::Private, token.span())?;
                        out.push(Ast::ExternFn(f));
                    }
                    TokenValue::Fn => {
                        let f = self.take_fn(Visibility::Private, token.span())?;
                        out.push(Ast::Fn(f));
                    }
                    TokenValue::Macro => {
                        let (ident_tok, ident) = expect_token!(self, MacroIdent(_));
                        let mut errors = Vec::new();
                        let args = if try_expect_token!(self, LParen).is_some() {
                            let mut args = Vec::new();
                            loop {
                                if try_expect_token!(self, RParen).is_some() {
                                    break;
                                }
                                if !args.is_empty() {
                                    expect_token!(self, Comma);
                                }
                                let (name_tok, name) = expect_token!(self, MacroVar(_));
                                let _ = expect_token!(self, Colon);
                                let (kind_tok, kind) = expect_token!(self, Ident(_));
                                let Some(kind) = MACRO_ARG_KIND.get(&kind) else {
                                    errors.push(ParseError::InvalidMacroArgKind {
                                        arg: name_tok.span(),
                                        kind,
                                        kind_span: kind_tok.span(),
                                    });
                                    continue;
                                };

                                args.push(MacroArg {
                                    name_span: name_tok.span(),
                                    name,
                                    kind: *kind,
                                });
                            }
                            Some(args)
                        } else {
                            None
                        };
                        let m = if let Some(open) = try_expect_token!(self, LCurly) {
                            let mut depth = 0;
                            let mut tokens = Vec::new();
                            let span = loop {
                                let body = match self.take_token() {
                                    Ok(Some(body)) => body,
                                    Ok(None) => {
                                        errors.push(ParseError::unexpected_eof(
                                            self.end_span(),
                                            &[],
                                            None,
                                        ));
                                        return Err(errors);
                                    }
                                    Err(e) => {
                                        errors.push(e.into());
                                        continue;
                                    }
                                };

                                match body.kind() {
                                    TokenKind::LCurly => {
                                        depth += 1;
                                        tokens.push(body);
                                    }
                                    TokenKind::RCurly if depth == 0 => {
                                        break open.span() + body.span();
                                    }
                                    TokenKind::RCurly => {
                                        depth -= 1;
                                        tokens.push(body);
                                    }
                                    _ => tokens.push(body),
                                }
                            };
                            Macro {
                                macro_span: token.span(),
                                ident_span: ident_tok.span(),
                                body_span: span,
                                tokens,
                                args,
                            }
                        } else {
                            match self.take_token() {
                                Ok(Some(body)) => Macro {
                                    macro_span: token.span(),
                                    ident_span: ident_tok.span(),
                                    body_span: body.span(),
                                    tokens: vec![body],
                                    args,
                                },
                                Ok(None) => {
                                    errors.push(ParseError::unexpected_eof(
                                        self.end_span(),
                                        &[],
                                        None,
                                    ));
                                    return Err(errors);
                                }
                                Err(e) => {
                                    errors.push(e.into());
                                    return Err(errors);
                                }
                            }
                        };

                        if !errors.is_empty() {
                            return Err(errors);
                        }

                        self.macros.insert(ident, m);
                    }
                    TokenValue::MacroIdent(ident) => {
                        if let Some(m) = self.macros.get(&ident) {
                            self.expand_macro(m.clone(), span)?;
                        } else {
                            // consume args, as that's probably what the user wants
                            let mut span = span;
                            if try_expect_token!(self, LParen).is_some() {
                                loop {
                                    if let Some(rparen) = try_expect_token!(self, RParen) {
                                        span += rparen.span();
                                        break;
                                    }
                                    let t = self.take_token().unwrap().unwrap();
                                    span += t.span();
                                }
                            }
                            return Err(vec![ParseError::UndefinedMacro { name: ident, span }]);
                        }
                    }
                    TokenValue::Pub => 'a: {
                        if let Some(extern_tok) = try_expect_token!(self, Extern) {
                            let f = self.take_extern_fn(
                                Visibility::Public,
                                token.span() + extern_tok.span(),
                            )?;
                            out.push(Ast::ExternFn(f));
                            break 'a;
                        }

                        if let Some(fn_tok) = try_expect_token!(self, Fn) {
                            let f =
                                self.take_fn(Visibility::Public, token.span() + fn_tok.span())?;
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

                        Err(vec![ParseError::unexpected_token(
                            token,
                            &[
                                TokenKind::Fn,
                                TokenKind::Use,
                                TokenKind::Mod,
                                TokenKind::Extern,
                            ],
                        )])?
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
                    TokenValue::As => Err(vec![ParseError::unexpected_token(token, &[])])?,
                }
                Result::<_, Vec<ParseError>>::Ok(())
            };
            match x() {
                Ok(_) => (),
                Err(e) => errors.extend(e),
            }
        }

        if errors.is_empty() {
            Ok(out)
        } else {
            Err(errors)
        }
    }
}
