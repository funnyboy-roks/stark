use std::{
    borrow::Cow,
    ffi::CString,
    num::{ParseFloatError, ParseIntError},
    ops::Range,
};

use miette::{Diagnostic, SourceSpan};
use phf::phf_map;
use thiserror::Error;

const KW_MAP: phf::Map<&'static str, TokenValue> = phf_map! {
    "dup" => TokenValue::Dup,
    "dup2" => TokenValue::Dup2,
    "swap" => TokenValue::Swap,
    "drop" => TokenValue::Drop,
    "extern" => TokenValue::Extern,
    "fn" => TokenValue::Fn,
    "then" => TokenValue::Then,
    "else" => TokenValue::Else,
    "while" => TokenValue::While,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NumLitVal {
    Integer(i64),
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

    // Ops
    Plus,
    Minus,
    Asterisk,
    Slash,
    Equal,
    Not,
    Lt,
    Percent,

    // Symbols
    /// `...`
    Ellipsis,
    /// `->`
    Arrow,

    // Keywords
    Dup,
    Dup2,
    Swap,
    Drop,
    Extern,
    Fn,

    Then,
    Else,
    While,
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

    // Symbols
    /// `...`
    Ellipsis,
    /// `->`
    Arrow,

    // Keywords
    Dup,
    Dup2,
    Swap,
    Drop,
    Extern,
    Fn,

    Then,
    Else,
    While,
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
            TokenValue::Plus => TokenKind::Plus,
            TokenValue::Minus => TokenKind::Minus,
            TokenValue::Asterisk => TokenKind::Asterisk,
            TokenValue::Slash => TokenKind::Slash,
            TokenValue::Equal => TokenKind::Equal,
            TokenValue::Not => TokenKind::Not,
            TokenValue::Lt => TokenKind::Lt,
            TokenValue::Percent => TokenKind::Percent,
            TokenValue::Ellipsis => TokenKind::Ellipsis,
            TokenValue::Arrow => TokenKind::Arrow,
            TokenValue::Dup => TokenKind::Dup,
            TokenValue::Dup2 => TokenKind::Dup2,
            TokenValue::Swap => TokenKind::Swap,
            TokenValue::Drop => TokenKind::Drop,
            TokenValue::Extern => TokenKind::Extern,
            TokenValue::Fn => TokenKind::Fn,
            TokenValue::Then => TokenKind::Then,
            TokenValue::Else => TokenKind::Else,
            TokenValue::While => TokenKind::While,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
        #[label = "here"]
        span: SourceSpan,
        #[source]
        source: ParseIntError,
    },
    #[error("Error parsing float")]
    FloatParseError {
        #[label = "here"]
        span: SourceSpan,
        #[source]
        source: ParseFloatError,
    },
    #[error("{} float literals are not supported", .kind)]
    UnsupportedFloat {
        #[label = "here"]
        span: SourceSpan,
        kind: String,
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
            span: (self.offset..self.offset + 1).into(),
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
            ('0', Some(c)) => {
                return Err(LexError::UnexpectedCharacter {
                    span: (self.offset..self.offset + 1).into(),
                    found: c,
                })
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
                span: (start..self.offset).into(),
                kind: match radix {
                    Radix::Bin => "Binary".into(),
                    Radix::Dec => unreachable!(),
                    Radix::Hex => "Hexadecimal".into(),
                },
            });
        }

        dbg!(float, string, radix);

        self.offset += string.len();
        if float {
            string
                .parse::<f64>()
                .map(|x| NumLit {
                    value: NumLitVal::Float(x * neg as f64),
                    radix,
                })
                .map_err(|e| LexError::FloatParseError {
                    span: (start..self.offset).into(),
                    source: e,
                })
        } else {
            i64::from_str_radix(string, radix.radix())
                .map(|x| NumLit {
                    value: NumLitVal::Integer(x * neg),
                    radix,
                })
                .map_err(|e| LexError::IntParseError {
                    span: (start..self.offset).into(),
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
                    return Some(
                        self.take_number().map(|n| {
                            Token::new(start..self.offset, self.file, TokenValue::NumLit(n))
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
                            Token::new(start..self.offset, self.file, TokenValue::NumLit(n))
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
                ('=', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Equal,
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
                ('<', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(
                        start..self.offset,
                        self.file,
                        TokenValue::Lt,
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
