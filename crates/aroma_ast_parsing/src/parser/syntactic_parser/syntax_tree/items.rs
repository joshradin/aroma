//! items, like functions, and classes

use crate::parser::singletons::{Abstract, Class as ClassTok, Private, Protected, Public};
use crate::parser::{CouldParse, Error, ErrorKind, Parsable, SyntacticParser};
use aroma_ast::id::Id;
use aroma_ast::token::{ToTokens, TokenKind};
use std::io::Read;

#[derive(Debug, ToTokens)]
pub enum Vis<'p> {
    Public(Public<'p>),
    Protected(Protected<'p>),
    Private(Private<'p>),
}

impl<'p> Parsable<'p> for Vis<'p> {
    type Err = Error<'p>;

    fn parse<R: Read>(
        parser: &mut SyntacticParser<'p, R>,
    ) -> Result<Self, crate::parser::Err<Self::Err>> {
        let next = parser
            .peek()?
            .cloned()
            .ok_or_else(|| parser.error(ErrorKind::UnexpectedEof, None))?;
        let vis = match next.kind() {
            TokenKind::Public => Vis::Public(parser.parse(Public::parse)?),

            TokenKind::Private => Vis::Private(parser.parse(Private::parse)?),
            TokenKind::Protected => Vis::Protected(parser.parse(Protected::parse)?),
            _ => {
                return Err(parser.error(
                    ErrorKind::expected_token(["public", "private", "protected"], next),
                    None,
                ));
            }
        };
        Ok(vis)
    }
}

impl<'p> CouldParse<'p> for Vis<'p> {
    fn could_parse<R: Read>(
        parser: &mut SyntacticParser<'p, R>,
    ) -> Result<bool, crate::parser::Err<Self::Err>> {
        Ok(parser
            .peek()?
            .map(|opt| {
                matches!(
                    opt.kind(),
                    TokenKind::Public | TokenKind::Private | TokenKind::Protected
                )
            })
            .unwrap_or(false))
    }
}

/// A class declaration
#[derive(Debug, ToTokens)]
pub struct Class<'p> {
    pub vis: Option<Vis<'p>>,
    pub abstract_: Option<Abstract<'p>>,
    pub class: ClassTok<'p>,
    pub id: Id<'p>,
}

impl<'p> Parsable<'p> for Class<'p> {
    type Err = Error<'p>;

    fn parse<R: Read>(
        parser: &mut SyntacticParser<'p, R>,
    ) -> Result<Self, crate::parser::Err<Self::Err>> {
        let vis = parser.parse_opt::<Vis>()?;

        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::syntactic_parser::tests::test_parser;

    #[test]
    fn test_parse_class() {
        test_parser(
            r#"
        public class Hello {

        }
        "#,
            |parser, _| {
                let class = parser.parse(Class::parse).expect("could not parse class");
            },
        )
    }
}
