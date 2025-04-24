use std::{borrow::Cow, num::ParseIntError, ops::Range};

use miette::{Diagnostic, SourceSpan};
use phf::phf_map;
use thiserror::Error;

const KW_MAP: phf::Map<&'static str, TokenValue> = phf_map! {
    "dup" => TokenValue::Dup,
    "drop" => TokenValue::Drop,
    "extern" => TokenValue::Extern,
    "fn" => TokenValue::Fn,
};

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
    IntLit,
    // FloatLit(i64), // TODO

    // Ops
    Plus,
    Minus,
    Asterisk,
    Slash,

    // Symbols
    /// `...`
    Ellipsis,
    /// `->`
    Arrow,

    // Keywords
    Dup,
    Drop,
    Extern,
    Fn,

    Then,
    Else,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenValue {
    Ident(String),
    LParen,
    RParen,
    LCurly,
    RCurly,
    Semicolon,

    // Lits
    StrLit(String),
    CStrLit(String),
    IntLit(i64),
    // FloatLit(i64), // TODO

    // Ops
    Plus,
    Minus,
    Asterisk,
    Slash,

    // Symbols
    /// `...`
    Ellipsis,
    /// `->`
    Arrow,

    // Keywords
    Dup,
    Drop,
    Extern,
    Fn,

    Then,
    Else,
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
            TokenValue::IntLit(_) => TokenKind::IntLit,
            TokenValue::Plus => TokenKind::Plus,
            TokenValue::Minus => TokenKind::Minus,
            TokenValue::Asterisk => TokenKind::Asterisk,
            TokenValue::Slash => TokenKind::Slash,
            TokenValue::Ellipsis => TokenKind::Ellipsis,
            TokenValue::Arrow => TokenKind::Arrow,
            TokenValue::Dup => TokenKind::Dup,
            TokenValue::Drop => TokenKind::Drop,
            TokenValue::Extern => TokenKind::Extern,
            TokenValue::Fn => TokenKind::Fn,
            TokenValue::Then => TokenKind::Then,
            TokenValue::Else => TokenKind::Else,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub file: String,
    pub span: SourceSpan,
    pub value: TokenValue,
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
        span: SourceSpan,
        found: char,
    },
    #[error("Unexpected end of file")]
    UnexpectedEof {
        #[label = "here"]
        span: SourceSpan,
    },
    #[error("Unterminated string literal")]
    UnterminatedStringLiteral {
        #[label = "here"]
        span: SourceSpan,
    },
    #[error("Error parsing integer")]
    IntParseError {
        #[label = "{source}"]
        span: SourceSpan,
        #[source]
        source: ParseIntError,
    },
}

impl Token {
    pub fn new(range: Range<usize>, file: impl Into<String>, kind: TokenValue) -> Token {
        Token {
            span: range.into(),
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
        let end = self.content[self.offset..]
            .find(|x: char| !x.is_whitespace())
            .unwrap_or(self.content.len());
        self.offset += end;
    }

    fn take_number(&mut self) -> Result<i64, LexError> {
        let start = self.offset;
        let x = self.content[self.offset + 1..] // skip the first char since we know it's fine
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(self.content.len());
        let content = &self.content[self.offset..][..x + 1];
        self.offset += content.len();
        content.parse().map_err(|e| LexError::IntParseError {
            span: (start..self.offset).into(),
            source: e,
        })
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
        let content = self.content[self.offset..].chars();
        let mut escaping = false;
        let mut owned: Option<String> = None;
        for c in content {
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
            span: (start - 1..self.content.len()).into(),
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
                span: (self.offset..self.offset + 1).into(),
            })?;
            let c2 = chars.next().ok_or(LexError::UnexpectedEof {
                span: (self.offset..self.offset + 1).into(),
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
            span: (self.offset..self.offset + 1).into(),
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
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
                    return Some(
                        self.take_number().map(|n| {
                            Token::new(start..self.offset, self.file, TokenValue::IntLit(n))
                        }),
                    )
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
                    return Some(
                        self.take_number().map(|n| {
                            Token::new(start..self.offset, self.file, TokenValue::IntLit(n))
                        }),
                    )
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
                        Token::new(start..self.offset, self.file, TokenValue::CStrLit(n))
                    }));
                }
                ('a'..='z' | 'A'..='Z' | '_', _) => {
                    return Some(self.take_ident().map(|n| {
                        Token::new(start..self.offset, self.file, TokenValue::from_ident(&*n))
                    }))
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
                ('"', _) => {
                    return Some(
                        self.take_strlit().map(|n| {
                            Token::new(start..self.offset, self.file, TokenValue::StrLit(n))
                        }),
                    )
                }
                (c, _) => {
                    return Some(Err(LexError::UnexpectedCharacter {
                        span: (start..self.offset + 1).into(),
                        found: c,
                    }))
                }
            }
        }
    }
}
