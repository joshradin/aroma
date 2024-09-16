//! Responsible with converting a [io::Read] obj into a token stream

use aroma_tokens::token::Token;
use nom::error::VerboseError;
use nom::Finish;
use std::io;
use std::io::{BufRead, Read};
use std::path::Path;
use std::str::Utf8Error;
use thiserror::Error;

mod token_parsing;
pub mod blocking;
mod non_blocking;
pub use non_blocking::*;

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
    use crate::lexer::blocking::Lexer;

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
