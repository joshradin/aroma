use super::{Error, ErrorKind, SyntacticParser};
use crate::parser::{CouldParse, Parse};
use aroma_ast::token::ToTokens;
use aroma_ast::token::Token;
use aroma_ast::token::TokenKind;
use aroma_ast::token::TokenStream;
use aroma_ast::spanned::Span;
use std::io::Read;

macro_rules! token_singleton {
    ($ty:ident, $($pat:tt)*) => {
        #[derive(Debug)]
        pub struct $ty<'p> {
            token: Token<'p>,
        }

        impl<'p> $ty<'p> {
            #[track_caller]
            pub fn new() -> Self {
                let token = Token::new(Span::call_site(), $($pat)*);
                Self { token }
            }
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
                if let Some(tok) = parser.consume_if(|token| matches!(token.kind(),  $($pat)*))? {
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
                    Ok(matches!(peek.kind(), $($pat)*))
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
token_singleton!(LShift, TokenKind::LShift);
token_singleton!(RShift, TokenKind::RShift);
token_singleton!(BitwiseAnd, TokenKind::BitwiseAnd);
token_singleton!(BitwiseOr, TokenKind::BitwiseOr);
token_singleton!(BitwiseXor, TokenKind::BitwiseXor);

token_singleton!(Eq, TokenKind::Eq);
token_singleton!(Neq, TokenKind::Neq);
token_singleton!(Lt, TokenKind::Lt);
token_singleton!(Lte, TokenKind::Lte);
token_singleton!(Gt, TokenKind::Gt);
token_singleton!(Gte, TokenKind::Gte);
token_singleton!(And, TokenKind::And);
token_singleton!(Or, TokenKind::Or);
token_singleton!(Not, TokenKind::Bang);

token_singleton!(MultAssign, TokenKind::MultAssign);
token_singleton!(PlusAssign, TokenKind::PlusAssign);
token_singleton!(MinusAssign, TokenKind::MinusAssign);
token_singleton!(DivAssign, TokenKind::DivAssign);
token_singleton!(RemnAssign, TokenKind::RemAssign);

token_singleton!(LParen, TokenKind::LParen);
token_singleton!(RParen, TokenKind::RParen);

token_singleton!(LBracket, TokenKind::LBracket);
token_singleton!(RBracket, TokenKind::RBracket);
token_singleton!(RCurly, TokenKind::RCurly);
token_singleton!(LCurly, TokenKind::LCurly);
token_singleton!(Arrow, TokenKind::Arrow);
token_singleton!(SemiC, TokenKind::SemiColon);

token_singleton!(Comma, TokenKind::Comma);
token_singleton!(Dot, TokenKind::Dot);
