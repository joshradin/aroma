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
token_singleton!(Comma, TokenKind::Comma);
token_singleton!(Dot, TokenKind::Dot);

#[derive(Debug)]
pub struct Punctuated<T, P> {
    pub punctuated: Vec<(T, Option<P>)>,
}

impl<T, P> Default for Punctuated<T, P> {
    fn default() -> Self {
        Self { punctuated: vec![] }
    }
}

impl<'p, T, P> ToTokens<'p> for Punctuated<T, P>
where
    T: ToTokens<'p>,
    P: ToTokens<'p>,
{
    fn to_tokens(&self) -> TokenStream<'p, 'p> {
        self.punctuated
            .iter()
            .flat_map(|(item, punc)| {
                item.to_tokens()
                    .chain(punc.iter().flat_map(|i| i.to_tokens()))
            })
            .collect()
    }
}

impl<'p, T, P> Parse<'p> for Punctuated<T, P>
where
    T: Parse<'p>,
    P: Parse<'p> + CouldParse<'p>,
    T::Err: From<P::Err>,
{
    type Err = T::Err;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, Self::Err> {
        let mut vec = vec![];
        loop {
            let v = T::parse(parser)?;
            if P::could_parse(parser)? {
                let punc = P::parse(parser)?;
                vec.push((v, Some(punc)));
            } else {
                vec.push((v, None));
                break;
            }
        }
        Ok(Punctuated { punctuated: vec })
    }
}
