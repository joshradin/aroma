use std::fmt::{Debug, Formatter};
use aroma_ast::token::{ToTokens, Token, TokenKind, TokenStream};
use std::io::Read;
use crate::parser;
use crate::parser::{ErrorKind, Parsable, SyntacticParser, SyntaxError};
use crate::parser::expr::remove_nl;

#[derive(Debug)]
pub enum ConstantKind {
    Float(f64),
    Integer(i64),
    String(String),
    Boolean(bool),
    Null,
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

impl<'p> Parsable<'p> for Constant<'p> {
    type Err = SyntaxError<'p>;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, parser::Err<Self::Err>> {
        parser.parse(remove_nl)?;
        if let Some(tok) = parser.consume_if(|token| {
            matches!(
                token.kind(),
                TokenKind::Float(_)
                    | TokenKind::Integer(_)
                    | TokenKind::Boolean(_)
                    | TokenKind::String(_)
                    | TokenKind::Null
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
                TokenKind::Null => Ok(Constant {
                    kind: ConstantKind::Null,
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