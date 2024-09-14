use aroma_tokens::spanned::{Span, Spanned};
use aroma_tokens::token::{ToTokens, TokenKind};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::io::Read;
use std::path::Path;

pub use crate::parser::error::*;
use tracing::trace;

pub mod blocking;
pub mod traits;

/// Err enum used to represent recoverable and non-recoverable errors
#[derive(Debug)]
pub enum Err<E> {
    /// a recoverable error
    Error(E),
    /// a non-recoverable error
    Failure(E),
}

impl<E> Err<E> {
    pub fn cut(self) -> Self {
        match self {
            Err::Error(e) => Err::Failure(e),
            e @ Err::Failure(_) => e,
        }
    }
}

impl<E: Error> Display for Err<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Err::Error(e) => {
                write!(f, "recoverable error: {e}")
            }
            Err::Failure(e) => {
                write!(f, "unrecoverable error: {e}")
            }
        }
    }
}

impl<E: Error> Error for Err<E> {}

impl<E> Err<E> {
    pub fn convert<E2>(self) -> Err<E2>
    where
        E: Into<E2>,
    {
        match self {
            Err::Error(e) => Err::Error(e.into()),
            Err::Failure(e) => Err::Failure(e.into()),
        }
    }
}

impl From<SyntaxError> for Err<SyntaxError> {
    fn from(value: SyntaxError) -> Self {
        Err::Error(value)
    }
}



#[cfg(test)]
pub(super) mod tests {
    use std::fs::File;
    use super::*;
    use crate::parser::blocking::BlockingParser;
    use crate::parser::hir::constants::{Constant, ConstantKind};
    use crate::parser::hir::cut;
    use crate::parser::hir::expr::Expr;
    use crate::parser::hir_parser::traits::blocking::Parsable;
    use aroma_ast::items::ClassItem;
    use aroma_tokens::spanned::{Span, Spanned};
    use aroma_tokens::token::{ToTokens, TokenKind};
    use std::io::Write as _;
    use tempfile::NamedTempFile;

    pub fn test_parser<F>(s: &str, callback: F)
    where
        F: FnOnce(&mut BlockingParser<File>, &Path),
    {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "{}", s).expect("could not write");
        let path = temp_file.path();
        let mut parser = BlockingParser::with_file(path).unwrap();
        callback(&mut parser, path)
    }
    #[test]
    fn test_create_parser_from_file() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "let x = 1.0;").expect("could not write");
        let path = temp_file.path();
        let mut parser = BlockingParser::with_file(path).unwrap();
        let token = parser.peek().unwrap().unwrap();
        assert_eq!(token.kind(), &TokenKind::Let);
        assert_eq!(token.span(), Span::new(temp_file.path(), 0, 3));
    }

    #[test]
    fn test_consume() {
        test_parser("let x = 1.0;", |parser, path| {
            let consumed_token = parser.consume().unwrap().unwrap();
            assert_eq!(consumed_token.kind(), &TokenKind::Let);
            assert_eq!(consumed_token.span(), Span::new(path, 0, 3));
            let token = parser.peek().unwrap().unwrap();
            assert_eq!(token.kind(), &TokenKind::Identifier("x".to_string()));
            assert_eq!(token.span(), Span::new(path, 4, 1));
        });
    }


    #[test]
    fn test_consume_if() {
        test_parser("let x = 1.0;", |parser, _| {
            assert!(parser
                .consume_if(|tok| tok.kind() == &TokenKind::MultAssign)
                .unwrap()
                .is_none());
            assert!(parser
                .consume_if(|tok| tok.kind() == &TokenKind::Let)
                .unwrap()
                .is_some());
        });
    }

    #[test]
    fn test_parse_constant() {
        test_parser("3.0", |parser, _| {
            let constant = parser.parse(Constant::parse).unwrap();
            assert!(matches!(constant.kind, ConstantKind::Float(3.0)));
            trace!(
                "{constant:?} -> {:#?}",
                constant.to_tokens().collect::<Vec<_>>()
            );
        });
        test_parser("3", |parser, _| {
            let constant = parser.parse(Constant::parse).unwrap();
            assert!(matches!(constant.kind, ConstantKind::Integer(3)));
            trace!(
                "{constant:?} -> {:#?}",
                constant.to_tokens().collect::<Vec<_>>()
            );
        })
    }
}

