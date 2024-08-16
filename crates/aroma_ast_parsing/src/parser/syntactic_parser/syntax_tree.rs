//! Syntax tree

use std::fmt::{Debug, Formatter};
use crate::parser::syntactic_parser::error::{Error, ErrorKind};
use crate::parser::SyntacticParser;
use aroma_ast::token::{ToTokens, Token, TokenKind, TokenStream};
use std::io::Read;
pub mod expr;
mod helpers;
pub mod singletons;

pub use helpers::*;

/// Parse a syntax tree part
pub trait Parse<'p>: ToTokens<'p> + Sized {
    type Err;

    /// Attempt to parse some syntax tree part
    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, Self::Err>;
}

/// A sub trait that determines if this type could be parsed without doing the parsing
pub trait CouldParse<'p>: Parse<'p> {
    /// Attempt to parse some syntax tree part
    fn could_parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<bool, Self::Err>;
}

#[derive(Debug)]
pub enum ConstantKind {
    Float(f64),
    Integer(i64),
    String(String),
    Boolean(bool),
}


pub struct Constant<'p> {
    pub kind: ConstantKind,
    pub tok: Token<'p>,
}

impl<'p> Debug for Constant<'p> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl<'p> Parse<'p> for Constant<'p> {
    type Err = Error<'p>;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> std::result::Result<Self, Self::Err> {
        if let Some(tok) = parser.consume_if(|token| {
            matches!(
                token.kind(),
                TokenKind::Float(_)
                    | TokenKind::Integer(_)
                    | TokenKind::Boolean(_)
                    | TokenKind::String(_)
            )
        })? {
            match tok.kind() {
                TokenKind::Float(f) => Ok(Constant {
                    kind: ConstantKind::Float(*f),
                    tok,
                }),
                TokenKind::Integer(f) => Ok(Constant {
                    kind: ConstantKind::Integer(*f),
                    tok,
                }),
                TokenKind::Boolean(f) => Ok(Constant {
                    kind: ConstantKind::Boolean(*f),
                    tok,
                }),
                TokenKind::String(f) => Ok(Constant {
                    kind: ConstantKind::String(f.clone()),
                    tok,
                }),
                _ => unreachable!(),
            }
        } else {
            let kind = ErrorKind::expected_token(["constant".to_string()], parser.consume()?);
            Err(parser.error(kind, None))
        }
    }
}

impl<'p> ToTokens<'p> for Constant<'p> {
    fn to_tokens(&self) -> TokenStream<'p, 'p> {
        TokenStream::from_iter([self.tok.clone()])
    }
}
