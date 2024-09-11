use aroma_tokens::token::{ToTokens, Token, TokenKind, TokenStream};
use std::fmt::{Debug, Formatter};
use std::io::Read;
use crate::parser::{Err, ErrorKind, blocking::SyntacticParser, SyntaxError};
use crate::parser::blocking::remove_nl;
use crate::parser::traits::Parsable;

#[derive(Debug)]
pub enum ConstantKind {
    Float(f64),
    Integer(i64),
    String(String),
    Boolean(bool),
    Null,
}

pub struct Constant {
    pub kind: ConstantKind,
    pub tok: Token,
}

impl Debug for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl Parsable for Constant {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> Result<Self, Err<Self::Err>> {
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
            let kind = ErrorKind::expected_token(
                ["{int}", "{float}", "true", "false", "null"],
                parser.consume()?,
            );
            Err(parser.error(kind, None))
        }
    }
}

impl ToTokens for Constant {
    fn to_tokens(&self) -> TokenStream {
        TokenStream::from_iter([self.tok.clone()])
    }
}
