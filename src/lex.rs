use std::{
    borrow::Cow,
    ffi::CString,
    num::{ParseFloatError, ParseIntError},
};

use miette::Diagnostic;
use phf::phf_map;
use thiserror::Error;

use crate::span::{Span, Spanned};

const KW_MAP: phf::Map<&'static str, TokenValue> = phf_map! {
    "dup" => TokenValue::Dup,
    "swap" => TokenValue::Swap,
    "drop" => TokenValue::Drop,
    "extern" => TokenValue::Extern,
    "fn" => TokenValue::Fn,
    "then" => TokenValue::Then,
    "else" => TokenValue::Else,
    "while" => TokenValue::While,
    "break" => TokenValue::Break,
    "true" => TokenValue::BoolLit(true),
    "false" => TokenValue::BoolLit(false),
    "cast" => TokenValue::Cast,
    "void" => TokenValue::Void,
    "load" => TokenValue::Load,
    "store" => TokenValue::Store,
    "pub" => TokenValue::Pub,
    "mod" => TokenValue::Mod,
    "use" => TokenValue::Use,
    "as" => TokenValue::As,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NumLitVal {
    Integer(i128),
    Float(f64),
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u32)]
pub enum Radix {
    Bin = 2,
    Dec = 10,
    Hex = 16,
}

impl Radix {
    pub fn radix(self) -> u32 {
        self as u32
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NumLit {
    pub value: NumLitVal,
    pub radix: Radix,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Ident,
    LParen,
    RParen,
    LCurly,
    RCurly,
    Semicolon,

    // Lits
    StrLit,
    CStrLit,
    NumLit,
    BoolLit,

    // Ops
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Asterisk,
    /// /
    Slash,
    /// =
    Equal,
    /// !
    Not,
    /// !=
    Neq,
    /// <
    Lt,
    /// <=
    Lte,
    /// >
    Gt,
    /// >=
    Gte,
    /// %
    Percent,

    // Symbols
    /// `...`
    Ellipsis,
    /// `->`
    Arrow,
    /// `::`
    PathSep,

    // Builtins
    Dup,
    Swap,
    Drop,
    Cast,
    Load,
    Store,

    // Keywords
    /// `extern`
    Extern,
    /// `fn`
    Fn,
    /// `void`
    Void,
    /// `pub`
    Pub,
    /// `mod`
    Mod,
    /// `use`
    Use,
    /// `as`
    As,

    Then,
    Else,
    While,
    Break,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    Ident(String),
    LParen,
    RParen,
    LCurly,
    RCurly,
    Semicolon,

    // Lits
    StrLit(String),
    CStrLit(CString),
    NumLit(NumLit),
    BoolLit(bool),

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

    // Symbols
    /// `...`
    Ellipsis,
    /// `->`
    Arrow,
    /// `::`
    PathSep,

    // Builtins
    Dup,
    Swap,
    Drop,
    Cast,
    Load,
    Store,

    // Keywords
    /// `extern`
    Extern,
    /// `fn`
    Fn,
    /// `void`
    Void,
    /// `pub`
    Pub,
    /// `mod`
    Mod,
    /// `use`
    Use,
    /// `as`
    As,

    Then,
    Else,
    While,
    Break,
}

impl TokenValue {
    fn from_ident(ident: &str) -> Self {
        KW_MAP
            .get(&ident.to_lowercase())
            .cloned() // This is cheap as it's just a unit variant
            .unwrap_or(TokenValue::Ident(ident.into()))
    }

    pub const fn kind(&self) -> TokenKind {
        match self {
            TokenValue::Ident(_) => TokenKind::Ident,
            TokenValue::LParen => TokenKind::LParen,
            TokenValue::RParen => TokenKind::RParen,
            TokenValue::LCurly => TokenKind::LCurly,
            TokenValue::RCurly => TokenKind::RCurly,
            TokenValue::Semicolon => TokenKind::Semicolon,
            TokenValue::StrLit(_) => TokenKind::StrLit,
            TokenValue::CStrLit(_) => TokenKind::CStrLit,
            TokenValue::NumLit(_) => TokenKind::NumLit,
            TokenValue::BoolLit(_) => TokenKind::BoolLit,
            TokenValue::Plus => TokenKind::Plus,
            TokenValue::Minus => TokenKind::Minus,
            TokenValue::Asterisk => TokenKind::Asterisk,
            TokenValue::Slash => TokenKind::Slash,
            TokenValue::Equal => TokenKind::Equal,
            TokenValue::Not => TokenKind::Not,
            TokenValue::Neq => TokenKind::Neq,
            TokenValue::Lt => TokenKind::Lt,
            TokenValue::Lte => TokenKind::Lte,
            TokenValue::Gt => TokenKind::Gt,
            TokenValue::Gte => TokenKind::Gte,
            TokenValue::Percent => TokenKind::Percent,
            TokenValue::Ellipsis => TokenKind::Ellipsis,
            TokenValue::PathSep => TokenKind::PathSep,
            TokenValue::Arrow => TokenKind::Arrow,
            TokenValue::Dup => TokenKind::Dup,
            TokenValue::Swap => TokenKind::Swap,
            TokenValue::Drop => TokenKind::Drop,
            TokenValue::Extern => TokenKind::Extern,
            TokenValue::Fn => TokenKind::Fn,
            TokenValue::Pub => TokenKind::Pub,
            TokenValue::Mod => TokenKind::Mod,
            TokenValue::Use => TokenKind::Use,
            TokenValue::As => TokenKind::As,
            TokenValue::Cast => TokenKind::Cast,
            TokenValue::Void => TokenKind::Void,
            TokenValue::Load => TokenKind::Load,
            TokenValue::Store => TokenKind::Store,
            TokenValue::Then => TokenKind::Then,
            TokenValue::Else => TokenKind::Else,
            TokenValue::While => TokenKind::While,
            TokenValue::Break => TokenKind::Break,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub file: String,
    span: Span,
    pub value: TokenValue,
}

impl Spanned for Token {
    fn span(&self) -> Span {
        self.span
    }
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        self.value.kind()
    }
}

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum LexError {
    #[error("Unexpected token '{found}'")]
    UnexpectedCharacter {
        #[label = "here"]
        span: Span,
        found: char,
    },
    #[error("Unexpected char '{found}', expected one of {expected:?}.")]
    UnexpectedCharacterExpected {
        #[label = "here"]
        span: Span,
        expected: Vec<char>,
        found: char,
    },
    #[error("Unexpected end of file")]
    UnexpectedEof {
        #[label = "here"]
        span: Span,
    },
    #[error("Unterminated string literal")]
    UnterminatedStringLiteral {
        #[label = "here"]
        span: Span,
    },
    #[error("Error parsing integer")]
    IntParseError {
        #[label = "here"]
        span: Span,
        #[source]
        source: ParseIntError,
    },
    #[error("Error parsing float")]
    FloatParseError {
        #[label = "here"]
        span: Span,
        #[source]
        source: ParseFloatError,
    },
    #[error("{} float literals are not supported", .kind)]
    UnsupportedFloat {
        #[label = "here"]
        span: Span,
        kind: String,
    },
}

impl Token {
    pub fn new(span: impl Into<Span>, file: impl Into<String>, kind: TokenValue) -> Token {
        Token {
            span: span.into(),
            file: file.into(),
            value: kind,
        }
    }
}

#[derive(Default, Clone)]
pub struct Lexer<'a> {
    content: &'a str,
    file: &'a str,
    offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(content: &'a str, file: &'a str) -> Self {
        Self {
            content,
            file,
            offset: 0,
        }
    }

    fn skip_whitespace(&mut self) {
        if self.offset < self.content.len() {
            let end = self.content[self.offset..]
                .find(|x: char| !x.is_whitespace())
                .unwrap_or(self.content.len());
            self.offset += end;
        }
    }

    fn take_number(&mut self) -> Result<NumLit, LexError> {
        let start = self.offset;
        let (num, neg) = if self.content[self.offset..]
            .chars()
            .next()
            .expect("Checked by the caller")
            == '-'
        {
            (&self.content[self.offset + 1..], -1)
        } else {
            (&self.content[self.offset..], 1)
        };

        let mut begin = num.chars();
        let first = begin.next().ok_or(LexError::UnexpectedEof {
            span: Span::new(self.offset..=self.offset),
        })?;
        let second = begin.next();

        let mut float = false;
        let (string, radix) = match (first, second) {
            ('0', Some('x')) => {
                let num = &num[2..];
                self.offset += 2;
                let x = num
                    .find(|c: char| !c.is_ascii_hexdigit())
                    .unwrap_or(num.len());
                (&num[..x], Radix::Hex)
            }
            ('0', Some('b')) => {
                let num = &num[2..];
                self.offset += 2;
                let x = num
                    .find(|c: char| !matches!(c, '0' | '1'))
                    .unwrap_or(num.len());
                (&num[..x], Radix::Bin)
            }
            ('0', Some('.')) => {
                let x = num
                    .find(|c: char| match c {
                        c if c.is_ascii_digit() => false,
                        '.' if !float => {
                            float = true;
                            false
                        }
                        _ => true,
                    })
                    .unwrap_or(num.len());
                (&num[..x], Radix::Dec)
            }
            ('0', Some(c @ ('a'..='z' | 'A'..='Z' | '_'))) => {
                return Err(LexError::UnexpectedCharacter {
                    span: Span::new(self.offset..=self.offset),
                    found: c,
                });
            }
            (_, _) => {
                let x = num
                    .find(|c: char| match c {
                        c if c.is_ascii_digit() => false,
                        '.' if !float => {
                            float = true;
                            false
                        }
                        _ => true,
                    })
                    .unwrap_or(num.len());
                (&num[..x], Radix::Dec)
            }
        };

        if float && radix != Radix::Dec {
            return Err(LexError::UnsupportedFloat {
                span: Span::new(start..self.offset),
                kind: match radix {
                    Radix::Bin => "Binary".into(),
                    Radix::Dec => unreachable!(),
                    Radix::Hex => "Hexadecimal".into(),
                },
            });
        }

        self.offset += string.len();
        if float {
            string
                .parse::<f64>()
                .map(|x| NumLit {
                    value: NumLitVal::Float(x * neg as f64),
                    radix,
                })
                .map_err(|e| LexError::FloatParseError {
                    span: Span::new(start..self.offset),
                    source: e,
                })
        } else {
            i128::from_str_radix(string, radix.radix())
                .map(|x| NumLit {
                    value: NumLitVal::Integer(x * neg),
                    radix,
                })
                .map_err(|e| LexError::IntParseError {
                    span: Span::new(start..self.offset),
                    source: e,
                })
        }
    }

    fn take_ident(&mut self) -> Result<Cow<'a, str>, LexError> {
        let x = self.content[self.offset..]
            .find(|c: char| !matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9' ))
            .unwrap_or(self.content.len());
        let content = &self.content[self.offset..][..x];
        self.offset += content.len();
        Ok(content.into())
    }

    fn take_strlit(&mut self) -> Result<String, LexError> {
        self.offset += 1;
        let start = self.offset;
        let mut end = self.offset;
        let mut content = self.content[self.offset..].chars().peekable();
        let mut escaping = false;
        let mut owned: Option<String> = None;
        while let Some(c) = content.next() {
            match c {
                '\\' if !escaping => {
                    if owned.is_none() {
                        owned = Some(self.content[start..end].to_string());
                    }
                    escaping = true;
                }
                '"' if !escaping => {
                    self.offset = end + 1;
                    if let Some(owned) = owned {
                        return Ok(owned);
                    } else {
                        return Ok(self.content[start..end].into());
                    }
                }
                'n' if escaping => {
                    if let Some(ref mut owned) = owned {
                        owned.push('\n');
                    }
                    escaping = false;
                }
                // TODO: support \0{33}
                '0' if escaping => {
                    let mut s = String::with_capacity(3);
                    for _ in 0..3 {
                        if let Some(a) = content.next_if(|c| matches!(c, '0'..='7')) {
                            s.push(a);
                            end += a.len_utf8();
                        } else {
                            break;
                        }
                    }
                    let c = if s.is_empty() {
                        '\0'
                    } else {
                        char::from(u8::from_str_radix(&s, 8).expect("TODO"))
                    };
                    owned.as_mut().unwrap().push(c);
                    escaping = false;
                }
                // TODO: support \x{1b}
                'x' if escaping => {
                    let mut s = String::with_capacity(2);
                    for _ in 0..2 {
                        if let Some(a) = content.next_if(|c| c.is_ascii_hexdigit()) {
                            s.push(a);
                            end += a.len_utf8();
                        } else {
                            break;
                        }
                    }
                    owned
                        .as_mut()
                        .unwrap()
                        .push(char::from(u8::from_str_radix(&s, 16).expect("TODO")));
                    escaping = false;
                }
                c => {
                    if let Some(ref mut owned) = owned {
                        owned.push(c);
                    }
                    escaping = false;
                }
            }
            end += c.len_utf8();
        }
        Err(LexError::UnterminatedStringLiteral {
            span: Span::new(start - 1..self.content.len()),
        })
    }

    fn skip_line(&mut self) {
        let newline = self.content[self.offset..].find('\n');
        if let Some(newline) = newline {
            self.offset += newline;
        } else {
            self.offset = self.content.len();
        }
    }

    /// Nested block comments:
    /// ```
    /// /* hello /* world */ */
    /// ```
    fn skip_block_comment(&mut self) -> Result<(), LexError> {
        let mut depth = 1;
        for i in self.offset..self.content.len() - 1 {
            if depth == 0 {
                return Ok(());
            }
            let mut chars = self.content[self.offset..].chars();
            let c1 = chars.next().ok_or(LexError::UnexpectedEof {
                span: Span::new(self.offset..=self.offset),
            })?;
            let c2 = chars.next().ok_or(LexError::UnexpectedEof {
                span: Span::new(self.offset..=self.offset),
            })?;

            match (c1, c2) {
                ('/', '*') => {
                    depth += 1;
                    self.offset += 1;
                }
                ('*', '/') => {
                    depth -= 1;
                    self.offset += 2;
                    if depth == 0 {
                        self.offset = i + 3;
                        return Ok(());
                    }
                }
                (_, _) => {
                    self.offset += 1;
                }
            }
        }
        Err(LexError::UnexpectedEof {
            span: Span::new(self.offset..=self.offset),
        })
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            self.skip_whitespace();
            if self.offset >= self.content.len() {
                return None;
            }
            let content = &self.content[self.offset..];
            let mut chars = content.chars();
            let c = chars.next()?;
            let c2 = chars.next();
            let start = self.offset;
            match (c, c2) {
                ('0'..='9', _) => {
                    return Some(self.take_number().map(|n| {
                        Token::new(start..self.offset, self.file, TokenValue::NumLit(n))
                    }));
                }
                ('/', Some('/')) => {
                    self.offset += 2;
                    self.skip_line();
                }
                ('/', Some('*')) => {
                    self.offset += 2;
                    if let Err(e) = self.skip_block_comment() {
                        return Some(Err(e));
                    };
                }
                ('-', Some('0'..='9')) => {
                    return Some(self.take_number().map(|n| {
                        Token::new(start..self.offset, self.file, TokenValue::NumLit(n))
                    }));
                }
                ('-', Some('>')) => {
                    self.offset += 2;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Arrow,
                    )));
                }
                ('.', Some('.')) if chars.next() == Some('.') => {
                    self.offset += 3;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Ellipsis,
                    )));
                }
                ('c', Some('"')) => {
                    self.offset += 1;
                    return Some(self.take_strlit().map(|n| {
                        Token::new(
                            start..self.offset,
                            self.file,
                            TokenValue::CStrLit(CString::new(n).unwrap()),
                        )
                    }));
                }
                ('a'..='z' | 'A'..='Z' | '_', _) => {
                    return Some(self.take_ident().map(|n| {
                        Token::new(start..self.offset, self.file, TokenValue::from_ident(&n))
                    }));
                }
                (':', Some(':')) => {
                    self.offset += 2;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::PathSep,
                    )));
                }
                (';', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Semicolon,
                    )));
                }
                ('+', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Plus,
                    )));
                }
                ('(', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::LParen,
                    )));
                }
                (')', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::RParen,
                    )));
                }
                ('{', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::LCurly,
                    )));
                }
                ('}', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::RCurly,
                    )));
                }
                ('-', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Minus,
                    )));
                }
                ('*', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Asterisk,
                    )));
                }
                ('/', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Slash,
                    )));
                }
                ('=', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Equal,
                    )));
                }
                ('!', Some('=')) => {
                    self.offset += 2;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Neq,
                    )));
                }
                ('!', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Not,
                    )));
                }
                ('<', Some('=')) => {
                    self.offset += 2;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Lte,
                    )));
                }
                ('<', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Lt,
                    )));
                }
                ('>', Some('=')) => {
                    self.offset += 2;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Gte,
                    )));
                }
                ('>', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Gt,
                    )));
                }
                ('%', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Percent,
                    )));
                }
                ('"', _) => {
                    return Some(self.take_strlit().map(|n| {
                        Token::new(start..self.offset, self.file, TokenValue::StrLit(n))
                    }));
                }
                (c, _) => {
                    return Some(Err(LexError::UnexpectedCharacter {
                        span: (start..self.offset + 1).into(),
                        found: c,
                    }));
                }
            }
        }
    }
}
