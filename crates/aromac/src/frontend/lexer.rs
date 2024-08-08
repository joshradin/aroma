//! Responsible with converting a [io::Read] obj into a token stream

use std::fs::File;
use std::io;
use std::io::{BufRead, Read};
use std::path::Path;
use std::str::Utf8Error;

use crate::common::spanned::Span;
use crate::frontend::lexer::nom_buf_reader::{BufReader, Parse, ParseError};
use crate::frontend::lexer::token_parsing::parse_token;
use crate::frontend::token::{Token, TokenKind};
use nom::Finish;
use thiserror::Error;

mod nom_buf_reader;
mod token_parsing;

/// Responsible with converting a [io::Read] obj into a token stream
#[derive(Debug)]
pub struct Lexer<'p, R> {
    path: &'p Path,
    reader: BufReader<R>,
    offset: usize,
}

impl<'p> Lexer<'p, File> {
    /// Creates a new lexer from a path
    pub fn read_path(path: &'p Path) -> io::Result<Self> {
        let reader = File::open(path)?;
        Self::new(path, reader)
    }
}

impl<'p, R: io::Read> Lexer<'p, R> {
    /// Creates a new lexer
    pub fn new(path: &'p Path, reader: R) -> io::Result<Self> {
        Ok(Self {
            path,
            reader: BufReader::new(reader),
            offset: 0,
        })
    }

    fn next_token(&mut self) -> LexResult<Option<Token<'p>>> {
        self.reader.fill_buf()?;
        match self.reader.parse(parse_token) {
            Ok((_, TokenKind::Eof)) => Ok(None),
            Ok((len, token_kind)) => {
                let offset = self.offset;
                self.offset += len;
                let span = Span::new(self.path, offset, len);
                Ok(Some(Token::new(span, token_kind)))
            }
            Err(e) => Err(e.into()),
        }
    }
}

impl<'p, R: Read> Iterator for Lexer<'p, R> {
    type Item = Result<Token<'p>, LexingError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(option) => option.map(Ok),
            Err(e) => Some(Err(e)),
        }
    }
}

type LexResult<T> = Result<T, LexingError>;

#[derive(Debug, Error)]
pub enum LexingError {
    /// Invalid char
    #[error("invalid char: {0}")]
    InvalidChar(char),
    #[error("unexpected EOF")]
    UnexpectedEof,
    #[error("{0:?} was not matched, got {1:?} instead")]
    CharNotMatched(char, char),
    #[error("{0:?} was not matched, got {1:?} instead")]
    StringNotMatched(String, String),
    #[error(transparent)]
    IoError(#[from] io::Error),
    #[error(transparent)]
    Utf8Error(#[from] Utf8Error),
    #[error(transparent)]
    NomError(#[from] ParseError<nom::error::Error<String>>),
    #[error(transparent)]
    VerboseNomError(#[from] ParseError<nom::error::VerboseError<String>>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let path = Path::new(file!());
        let f = 1.0e10;
        let test = "const v: T = 0; { print \"hello, world\"; i = 1.0e10 }";
        let mut buffer = Vec::from(test);
        let mut lexer = Lexer::new(path, &*buffer).unwrap();
        let tokens = match lexer.try_fold(
            vec![],
            |mut accum, next| -> Result<Vec<Token>, LexingError> {
                accum.push(next?);
                Ok(accum)
            },
        ) {
            Ok(tokens) => tokens,
            Err(e) => panic!("{} -> {e:#?}", e),
        };
        assert!(tokens.len() >= 1);
        println!("{test} => {tokens:?}");
    }
}
