use aroma_tokens::id::Id;
use std::io::Read;
use std::error::Error;
use aroma_tokens::token::{ToTokens, TokenKind};
use std::result;
use std::any::type_name;
use crate::parser;
use crate::parser::hir::{map, Punctuated1};
use crate::parser::hir::singletons::{Dot, VarId};
use crate::parser::{ErrorKind, blocking::SyntacticParser, SyntaxError, SyntaxResult};

/// Parser for syntax tree items
pub trait Parser<R: Read, O, E = SyntaxError>: Clone
where
    E: std::error::Error,
{
    fn non_terminal(&self) -> &'static str;
    fn parse(&mut self, parser: &mut SyntacticParser<'_, R>) -> result::Result<O, parser::Err<E>>;
}

impl<R, O, E, F> Parser<R, O, E> for F
where
    R: Read,
    F: FnMut(&mut SyntacticParser<'_, R>) -> result::Result<O, parser::Err<E>>,
    F: Clone,
    E: std::error::Error,
{
    fn non_terminal(&self) -> &'static str {
        type_name::<F>()
    }

    fn parse(&mut self, parser: &mut SyntacticParser<'_, R>) -> result::Result<O, parser::Err<E>> {
        (self)(parser)
    }
}

/// Parse a syntax tree part
pub trait Parsable: ToTokens + Sized {
    type Err;

    /// Attempt to parse some syntax tree part
    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> Result<Self, parser::Err<Self::Err>>;
}

/// A sub trait that determines if this type could be parsed without doing the parsing
pub trait CouldParse: Parsable {
    /// Attempt to parse some syntax tree part
    fn could_parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> Result<bool, parser::Err<Self::Err>>;
}

impl<P: Parsable + CouldParse> Parsable for Vec<P>
where
    P::Err: Error,
{
    type Err = P::Err;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> Result<Self, parser::Err<Self::Err>> {
        let mut result = Vec::new();
        while let Some(item) = parser.parse_opt::<P>()? {
            result.push(item);
        }

        Ok(result)
    }
}

impl<P: Parsable + CouldParse> Parsable for Option<P>
where
    P::Err: Error,
{
    type Err = P::Err;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> Result<Self, parser::Err<Self::Err>> {
        parser.parse_opt::<P>()
    }
}

impl Parsable for Id {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
        let parts = map(Punctuated1::<VarId, Dot>::parse, |ids| {
            Id::new(
                ids.punctuated
                    .into_iter()
                    .flat_map(|(var, _)| var.id.to_tokens()),
            )
            .unwrap()
        });
        parser
            .try_parse(parts)
            .map_err(parser::Err::Error)
            .and_then(|id| id.ok_or_else(|| parser.error(ErrorKind::UnexpectedEof, None)))
    }
}

impl CouldParse for Id {
    fn could_parse<R: Read>(parser: &mut SyntacticParser<R>) -> SyntaxResult<bool> {
        Ok(parser
            .peek()?
            .map(|t| matches!(t.kind(), TokenKind::Identifier(_)))
            .unwrap_or(false))
    }
}