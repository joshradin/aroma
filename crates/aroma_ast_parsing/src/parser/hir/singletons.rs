use super::{ErrorKind, SyntaxError};
use crate::parser::blocking::{remove_nl, BlockingParser};
use crate::parser::hir_parser::blocking::{CouldParse, Parsable};
use crate::parser::Err;
use aroma_tokens::id::Id;
use aroma_tokens::spanned::Span;
use aroma_tokens::spanned::Spanned;
use aroma_tokens::token::ToTokens;
use aroma_tokens::token::Token;
use aroma_tokens::token::TokenKind;
use aroma_tokens::token::TokenStream;
use std::io::Read;

/// A variable id, an Id with a signle id
#[derive(Debug, ToTokens)]
pub struct VarId {
    pub id: Id,
}

impl Parsable for VarId {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> crate::parser::SyntaxResult<Self> {
        if let Some(tok) =
            parser.consume_if(|tok| matches!(tok.kind(), TokenKind::Identifier(_)))?
        {
            Ok(VarId {
                id: Id::new([tok]).expect("should not fail"),
            })
        } else {
            Err(parser.error(ErrorKind::expected_token(["id"], None), None))
        }
    }
}

impl AsRef<str> for VarId {
    fn as_ref(&self) -> &str {
        self.id.try_as_ref().unwrap()
    }
}

#[derive(Debug)]
pub struct DocComment {
    pub token: Token,
}

impl DocComment {
    pub fn comment(&self) -> &str {
        let TokenKind::DocComment(comment) = self.token.kind() else {
            unreachable!()
        };
        comment.as_str()
    }
}

impl ToTokens for DocComment {
    fn to_tokens(&self) -> TokenStream {
        TokenStream::from_iter([self.token.clone()])
    }
}

impl Parsable for DocComment {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> Result<Self, Err<Self::Err>> {
        parser.parse(remove_nl)?;
        if let Some(tok) =
            parser.consume_if(|token| matches!(token.kind(), TokenKind::DocComment(_)))?
        {
            Ok(Self { token: tok })
        } else {
            let tok = parser.consume()?;
            let span = tok.as_ref().map(|t| t.span());
            let kind = ErrorKind::expected_token([stringify!(DocComment)], tok);
            match span {
                Some(span) => Err(parser.error_with_span(kind, None, span)),
                None => Err(parser.error(kind, None)),
            }
        }
    }
}

impl CouldParse for DocComment {
    fn could_parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> Result<bool, Err<Self::Err>> {
        if let Some(peek) = parser.peek()? {
            Ok(matches!(peek.kind(), TokenKind::DocComment(_)))
        } else {
            Ok(false)
        }
    }
}

macro_rules! token_singleton {
    ($ty:ident, $($pat:tt)*) => {
        #[derive(Debug, Clone, PartialEq)]
        pub struct $ty {
            token: Token,
        }

        impl Default for $ty {
            fn default() -> Self {
                Self::new()
            }
        }

        impl $ty {
            #[track_caller]
            pub fn new() -> Self {
                let token = Token::new(Span::call_site(), $($pat)*);
                Self { token }
            }
        }

        impl TryFrom<Token> for $ty {
            type Error = SyntaxError;

            fn try_from(token: Token) -> Result<Self, Self::Error> {
                match token.kind() {
                    $($pat)* => { Ok(Self { token }) },
                    _ => Err(ErrorKind::ExpectedToken(vec![format!("{:?}", $($pat)*)], Some(token)).into())
                }
            }
        }

        #[automatically_derived]
        impl ToTokens for $ty {
            fn to_tokens(&self) -> TokenStream {
                TokenStream::from_iter([self.token.clone()])
            }
        }

        #[automatically_derived]
        impl Parsable for $ty {
            type Err = SyntaxError;

            fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> Result<Self, Err<Self::Err>> {
                if !matches!($($pat)*, TokenKind::Nl) {
                    parser.parse(remove_nl)?;
                }
                if let Some(tok) = parser.consume_if(|token| matches!(token.kind(),  $($pat)*))? {
                    Ok($ty { token: tok })
                } else {
                    let tok = parser.consume()?;
                    let span = tok.as_ref().map(|t| t.span());
                    let kind = ErrorKind::expected_token([stringify!($ty)], tok);
                    match span {
                        Some(span) => {
                             Err(parser.error_with_span(kind, None, span))
                        }
                        None => {
                             Err(parser.error(kind, None))
                        }
                    }

                }
            }
        }

        #[automatically_derived]
        impl CouldParse for $ty {
            fn could_parse<R: Read>(
                parser: &mut BlockingParser<'_, R>,
            ) -> Result<bool, Err<Self::Err>> {
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
token_singleton!(RemAssign, TokenKind::RemAssign);
token_singleton!(Assign, TokenKind::Assign);

token_singleton!(LParen, TokenKind::LParen);
token_singleton!(RParen, TokenKind::RParen);

token_singleton!(LBracket, TokenKind::LBracket);
token_singleton!(RBracket, TokenKind::RBracket);
token_singleton!(RCurly, TokenKind::RCurly);
token_singleton!(LCurly, TokenKind::LCurly);
token_singleton!(Arrow, TokenKind::Arrow);
token_singleton!(SemiC, TokenKind::SemiColon);
token_singleton!(Nl, TokenKind::Nl);

token_singleton!(Comma, TokenKind::Comma);
token_singleton!(Dot, TokenKind::Dot);
token_singleton!(Colon, TokenKind::Colon);
token_singleton!(QMark, TokenKind::QMark);

token_singleton!(Class, TokenKind::Class);
token_singleton!(Interface, TokenKind::Interface);
token_singleton!(Abstract, TokenKind::Abstract);
token_singleton!(Let, TokenKind::Let);
token_singleton!(Const, TokenKind::Const);
token_singleton!(In, TokenKind::In);
token_singleton!(Out, TokenKind::Out);
token_singleton!(If, TokenKind::If);
token_singleton!(Else, TokenKind::Else);
token_singleton!(While, TokenKind::While);
token_singleton!(For, TokenKind::For);
token_singleton!(Try, TokenKind::Try);
token_singleton!(Catch, TokenKind::Catch);
token_singleton!(Match, TokenKind::Match);
token_singleton!(Loop, TokenKind::Loop);
token_singleton!(Break, TokenKind::Break);
token_singleton!(Continue, TokenKind::Continue);
token_singleton!(Return, TokenKind::Return);

token_singleton!(This, TokenKind::This);
token_singleton!(Super, TokenKind::Super);
token_singleton!(Delegate, TokenKind::Delegate);

token_singleton!(Public, TokenKind::Public);
token_singleton!(Protected, TokenKind::Protected);
token_singleton!(Private, TokenKind::Private);
token_singleton!(Extends, TokenKind::Extends);
token_singleton!(Implements, TokenKind::Implements);
token_singleton!(Static, TokenKind::Static);
token_singleton!(Fn, TokenKind::Fn);
token_singleton!(Final, TokenKind::Final);
token_singleton!(Throws, TokenKind::Throws);
token_singleton!(Constructor, TokenKind::Constructor);

token_singleton!(Namespace, TokenKind::Namespace);
token_singleton!(Import, TokenKind::Import);
token_singleton!(Hash, TokenKind::Hash);
