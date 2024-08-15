use super::{Error, ErrorKind, SyntacticParser};
use crate::parser::{CouldParse, Parse};
use aroma_ast::token::ToTokens;
use aroma_ast::token::Token;
use aroma_ast::token::TokenKind;
use aroma_ast::token::TokenStream;
use std::io::Read;

macro_rules! token_singleton {
    ($ty:ident, $pat:pat) => {
        #[derive(Debug)]
        pub struct $ty<'p> {
            token: Token<'p>,
        }

        #[automatically_derived]
        impl<'p> ToTokens<'p> for $ty<'p> {
            fn to_tokens(&self) -> TokenStream<'p, 'p> {
                TokenStream::from_iter([self.token.clone()])
            }
        }

        #[automatically_derived]
        impl<'p> Parse<'p> for $ty<'p> {
            type Err = Error<'p>;

            fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, Self::Err> {
                if let Some(tok) = parser.consume_if(|token| matches!(token.kind(), $pat))? {
                    Ok($ty { token: tok })
                } else {
                    let kind = ErrorKind::expected_token([stringify!($ty)], parser.consume()?);
                    Err(parser.error(kind, None))
                }
            }
        }

        #[automatically_derived]
        impl<'p> CouldParse<'p> for $ty<'p> {
            fn could_parse<R: Read>(
                parser: &mut SyntacticParser<'p, R>,
            ) -> Result<bool, Self::Err> {
                if let Some(peek) = parser.peek()? {
                    Ok(matches!(peek.kind(), $pat))
                } else {
                    Ok(false)
                }
            }
        }
    };
}

token_singleton!(Plus, TokenKind::Plus);
token_singleton!(Sub, TokenKind::Minus);
token_singleton!(Mult, TokenKind::Star);
token_singleton!(Div, TokenKind::Div);
token_singleton!(Rem, TokenKind::Rem);

token_singleton!(LParen, TokenKind::LParen);
token_singleton!(RParen, TokenKind::RParen);

token_singleton!(LBracket, TokenKind::LBracket);
token_singleton!(RBracket, TokenKind::RBracket);

token_singleton!(Comma, TokenKind::Comma);
token_singleton!(Dot, TokenKind::Dot);
