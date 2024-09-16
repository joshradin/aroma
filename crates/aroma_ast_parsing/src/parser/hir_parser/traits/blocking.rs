use crate::parser;
use crate::parser::blocking::BlockingParser;
use crate::parser::SyntaxError;
use aroma_tokens::token::ToTokens;
use std::any::type_name;
use std::error::Error;
use std::io::Read;

/// Parser for syntax tree items
pub trait Parser<R: Read, O, E = SyntaxError>: Clone
where
    E: Error,
{
    fn non_terminal(&self) -> &'static str;
    fn parse(&mut self, parser: &mut BlockingParser<'_, R>) -> Result<O, parser::Err<E>>;
}

impl<R, O, E, F> Parser<R, O, E> for F
where
    R: Read,
    F: FnMut(&mut BlockingParser<'_, R>) -> Result<O, parser::Err<E>>,
    F: Clone,
    E: Error,
{
    fn non_terminal(&self) -> &'static str {
        type_name::<O>()
    }

    fn parse(&mut self, parser: &mut BlockingParser<'_, R>) -> Result<O, parser::Err<E>> {
        (self)(parser)
    }
}

/// Parse a syntax tree part
pub trait Parsable: ToTokens + Sized {
    type Err;

    /// Attempt to parse some syntax tree part
    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> Result<Self, parser::Err<Self::Err>>;
}

/// A sub trait that determines if this type could be parsed without doing the parsing
pub trait CouldParse: Parsable {
    /// Attempt to parse some syntax tree part
    fn could_parse<R: Read>(
        parser: &mut BlockingParser<'_, R>,
    ) -> Result<bool, parser::Err<Self::Err>>;
}

impl<P: Parsable + CouldParse> Parsable for Vec<P>
where
    P::Err: Error,
{
    type Err = P::Err;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> Result<Self, parser::Err<Self::Err>> {
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

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> Result<Self, parser::Err<Self::Err>> {
        parser.parse_opt::<P>()
    }
}