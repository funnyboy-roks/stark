use std::{
    borrow::Cow,
    error::Error,
    fmt::Display,
    num::{IntErrorKind, ParseIntError},
    ops::Range,
};

use phf::phf_map;

const KW_MAP: phf::Map<&'static str, TokenKind<'_>> = phf_map! {
    "dup" => TokenKind::Dup,
    "drop" => TokenKind::Drop,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind<'a> {
    Ident(Cow<'a, str>),

    // Lits
    StrLit(Cow<'a, str>),
    IntLit(i64),
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

impl<'a> TokenKind<'a> {
    fn from_ident(ident: Cow<'a, str>) -> Self {
        KW_MAP
            .get(&ident.to_lowercase())
            .cloned() // This is cheap as it's just a unit variant
            .unwrap_or(TokenKind::Ident(ident))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token<'a> {
    pub offset: usize,
    pub end: usize,
    pub kind: TokenKind<'a>,
}

#[derive(Debug, Clone)]
pub enum LexError {
    UnexpectedCharacter(char),
    UnexpectedEof,
    IntParseError(IntErrorKind),
}

impl From<ParseIntError> for LexError {
    fn from(value: ParseIntError) -> Self {
        Self::IntParseError(value.kind().clone())
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for LexError {
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

impl Token<'_> {
    pub fn new(range: Range<usize>, kind: TokenKind<'_>) -> Token<'_> {
        Token {
            offset: range.start,
            end: range.end,
            kind,
        }
    }
}

#[derive(Default, Clone)]
pub struct Lexer<'a> {
    content: &'a str,
    offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(content: &str) -> Lexer<'_> {
        Lexer { content, offset: 0 }
    }

    fn skip_whitespace(&mut self) {
        let end = self.content[self.offset..]
            .find(|x: char| !x.is_whitespace())
            .unwrap_or(self.content.len());
        self.offset += end;
    }

    fn take_number(&mut self) -> Result<i64, LexError> {
        let x = self.content[self.offset + 1..] // skip the first char since we know it's fine
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(self.content.len());
        let content = &self.content[self.offset..][..x + 1];
        self.offset += content.len();
        Ok(content.parse()?)
    }

    fn take_ident(&mut self) -> Result<Cow<'a, str>, LexError> {
        let x = self.content[self.offset..]
            .find(|c: char| !matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9' ))
            .unwrap_or(self.content.len());
        let content = &self.content[self.offset..][..x];
        self.offset += content.len();
        Ok(content.into())
    }

    fn take_strlit(&mut self) -> Result<Cow<'a, str>, LexError> {
        self.offset += 1;
        let start = self.offset;
        let mut end = self.offset;
        let content = self.content[self.offset..].chars();
        // TODO: support escaping of strings
        for c in content {
            if c == '"' {
                self.offset = end + 1;
                return Ok(Cow::Borrowed(&self.content[start..end]));
            } else {
                end += c.len_utf8();
            }
        }
        Err(LexError::UnexpectedEof)
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
            let c1 = chars.next().ok_or(LexError::UnexpectedEof)?;
            let c2 = chars.next().ok_or(LexError::UnexpectedEof)?;

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
        Err(LexError::UnexpectedEof)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexError>;

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
                        self.take_number()
                            .map(|n| Token::new(start..self.offset, TokenKind::IntLit(n))),
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
                        self.take_number()
                            .map(|n| Token::new(start..self.offset, TokenKind::IntLit(n))),
                    )
                }
                ('a'..='z' | 'A'..='Z' | '_', _) => {
                    return Some(
                        self.take_ident()
                            .map(|n| Token::new(start..self.offset, TokenKind::from_ident(n))),
                    )
                }
                ('+', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(start..self.offset, TokenKind::Plus)));
                }
                ('-', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(start..self.offset, TokenKind::Minus)));
                }
                ('*', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(start..self.offset, TokenKind::Asterisk)));
                }
                ('/', _) => {
                    self.offset += 1;
                    return Some(Ok(Token::new(start..self.offset, TokenKind::Slash)));
                }
                ('"', _) => {
                    return Some(
                        self.take_strlit()
                            .map(|n| Token::new(start..self.offset, TokenKind::StrLit(n))),
                    )
                }
                (c, _) => return Some(Err(LexError::UnexpectedCharacter(c))),
            }
        }
    }
}
