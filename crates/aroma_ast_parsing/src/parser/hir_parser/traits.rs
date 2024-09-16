use crate::parser;
use crate::parser::hir::singletons::{Dot, VarId};
use crate::parser::hir::{map, Punctuated1};
use crate::parser::{blocking::BlockingParser, ErrorKind, SyntaxError, SyntaxResult};
use aroma_tokens::id::Id;
use aroma_tokens::token::{ToTokens, TokenKind};
use blocking::{CouldParse, Parsable};
use std::error::Error;
use std::future::{Future, IntoFuture};
use std::io::Read;

pub(super) mod blocking;


impl Parsable for Id {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
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
    fn could_parse<R: Read>(parser: &mut BlockingParser<R>) -> SyntaxResult<bool> {
        Ok(parser
            .peek()?
            .map(|t| matches!(t.kind(), TokenKind::Identifier(_)))
            .unwrap_or(false))
    }
}
