//! Responsible with converting a [io::Read] obj into a token stream

use crate::lexer::token_parsing::parse_token;
use aroma_ast::spanned::Span;
use aroma_ast::token::{Token, TokenKind};
use nom::error::VerboseError;
use nom::{Finish, Needed};
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Read};
use std::path::Path;
use std::str::Utf8Error;
use thiserror::Error;

mod token_parsing;

/// Responsible with converting a [Read] obj into a token stream
#[derive(Debug)]
pub struct Lexer<'p, R> {
    path: &'p Path,
    buffer: Vec<u8>,
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

impl<'p, R: Read> Lexer<'p, R> {
    /// Creates a new lexer
    pub fn new(path: &'p Path, reader: R) -> io::Result<Self> {
        Ok(Self {
            path,
            buffer: vec![],
            reader: BufReader::new(reader),
            offset: 0,
        })
    }

    fn next_token(&mut self) -> LexResult<Option<Token>> {
        if self.buffer.is_empty() {
            self.buffer = vec![0; 4096];
            let read = self.reader.read(&mut self.buffer)?;
            self.buffer = self.buffer.drain(..read).collect();
        }
        while !self.buffer.is_empty() {
            let as_str = std::str::from_utf8(&self.buffer)?;
            match parse_token(as_str) {
                Ok((rest, (_, _, _, TokenKind::Eof))) => return Ok(None),
                Ok((rest, (l, len, r, token_kind))) => {
                    let offset = self.offset + l;
                    self.offset += l + len + r;
                    let span = Span::new(self.path, offset, len);
                    self.buffer = Vec::from(rest);
                    return Ok(Some(Token::new(span, token_kind)));
                }
                Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => return Err(e.into()),
                Err(nom::Err::Incomplete(needed)) => match needed {
                    Needed::Unknown => {
                        let len = (self.buffer.len() * 2).min(8);
                        let mut buffer = vec![0_u8; len];
                        let read = self.reader.read(&mut self.buffer)?;
                        self.buffer.extend(buffer.drain(..read));
                    }
                    Needed::Size(size) => {
                        let mut buffer = vec![0_u8; size.get()];
                        let read = self.reader.read(&mut buffer)?;
                        self.buffer.extend(buffer.drain(..read));
                    }
                },
            }
        }
        Ok(None)

        // match self.reader.li.parse(parse_token) {
        //     Ok((_, _, _, TokenKind::Eof)) => Ok(None),
        //     Ok((l, len, r, token_kind)) => {
        //         let offset = self.offset + l;
        //         self.offset += l + len + r;
        //         let span = Span::new(self.path, offset, len);
        //         Ok(Some(Token::new(span, token_kind)))
        //     }
        //     Err(e) => Err(e.into()),
        // }
    }
}

impl<'p, R: Read> Iterator for Lexer<'p, R> {
    type Item = Result<Token, LexingError>;

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
    NomError(#[from] VerboseError<String>),
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
