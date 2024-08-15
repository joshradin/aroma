//! Syntax tree

use crate::parser::syntactic_parser::error::{Error, ErrorKind};
use crate::parser::SyntacticParser;
use aroma_ast::id::Id;
use aroma_ast::token::{ToTokens, Token, TokenKind, TokenStream};
use std::io::Read;

mod helpers;

pub use helpers::*;

/// Parse a syntax tree part
pub trait Parse<'p>: ToTokens<'p> + Sized {
    type Err;

    /// Attempt to parse some syntax tree part
    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, Self::Err>;
}

/// A sub trait that determines if this type could be parsed without doing the parsing
pub trait CouldParse<'p>: Parse<'p> {
    /// Attempt to parse some syntax tree part
    fn could_parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<bool, Self::Err>;
}

#[derive(Debug)]
pub enum ConstantKind {
    Float(f64),
    Integer(i64),
    String(String),
    Boolean(bool),
}

#[derive(Debug)]
pub struct Constant<'p> {
    pub kind: ConstantKind,
    pub tok: Token<'p>,
}
impl<'p> Parse<'p> for Constant<'p> {
    type Err = Error<'p>;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> std::result::Result<Self, Self::Err> {
        if let Some(tok) = parser.consume_if(|token| {
            matches!(
                token.kind(),
                TokenKind::Float(_)
                    | TokenKind::Integer(_)
                    | TokenKind::Boolean(_)
                    | TokenKind::String(_)
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

/// An expression
#[derive(Debug)]
pub enum Expr<'p> {
    Path(Id<'p>),
    Constant(Constant<'p>),
    Binary(ExprBinary<'p>),
    Field(FieldExpr<'p>),
    Call(CallExpr<'p>),
    Index(),
    Unary(),
}

impl<'p> Parse<'p> for Expr<'p> {
    type Err = Error<'p>;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, Self::Err> {
        parse_expr(parser)
    }
}

impl<'p> ToTokens<'p> for Expr<'p> {
    fn to_tokens(&self) -> TokenStream<'p, 'p> {
        match self {
            Expr::Path(i) => i.to_tokens(),
            Expr::Constant(c) => c.to_tokens(),
            Expr::Binary(binary) => binary.to_tokens(),
            Expr::Call(call) => call.to_tokens(),
            _ => todo!(),
        }
    }
}

fn parse_expr<'p>(parser: &mut SyntacticParser<'p, impl Read>) -> Result<Expr<'p>, Error<'p>> {
    let mut l = parse_group(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::Plus | TokenKind::Minus))
    {
        let binop = parser.parse::<BinOp>()?;
        let r = parse_group(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}

fn parse_group<'p>(parser: &mut SyntacticParser<'p, impl Read>) -> Result<Expr<'p>, Error<'p>> {
    let mut l = parse_call(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::Star | TokenKind::Div))
    {
        let binop = parser.parse::<BinOp>()?;
        let r = parse_call(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}

fn parse_call<'p>(parser: &mut SyntacticParser<'p, impl Read>) -> Result<Expr<'p>, Error<'p>> {
    let mut callee = parse_primary(parser)?;
    while LParen::could_parse(parser)? {
        let lparen = parser.parse::<LParen>()?;
        let punctuated = if RParen::could_parse(parser)? {
            Punctuated::default()
        } else {
            parser.parse::<Punctuated<_, _>>()?
        };
        let rparen = parser.parse::<RParen>()?;
        callee = Expr::Call(CallExpr {
            callee: Box::new(callee),
            lparen,
            parameters: punctuated,
            rparen,
        });
    }
    Ok(callee)
}

fn parse_primary<'p>(parser: &mut SyntacticParser<'p, impl Read>) -> Result<Expr<'p>, Error<'p>> {
    match parser.peek()? {
        Some(t)
            if matches!(
                t.kind(),
                TokenKind::Float(_)
                    | TokenKind::Integer(_)
                    | TokenKind::String(_)
                    | TokenKind::Boolean(_)
            ) =>
        {
            let constant = parser.parse::<Constant>()?;
            Ok(Expr::Constant(constant))
        }
        Some(t) if matches!(t.kind(), TokenKind::Identifier(_)) => {
            let id = parser.parse::<Id>()?;
            Ok(Expr::Path(id))
        }
        Some(t) if matches!(t.kind(), TokenKind::LParen) => {
            parser.consume()?;
            let expr = parser.parse::<Expr>()?;
            parser
                .consume_if(|token| matches!(token.kind(), TokenKind::RParen))?
                .ok_or_else(|| {
                    let kind =
                        ErrorKind::expected_token([")".to_string()], parser.consume().unwrap());
                    parser.error(kind, None)
                })?;
            Ok(expr)
        }
        _ => {
            let kind = ErrorKind::expected_token(
                [
                    "constant".to_string(),
                    "identifier".to_string(),
                    "(".to_string(),
                ],
                parser.consume()?,
            );
            Err(parser.error(kind, None))
        }
    }
}

#[derive(Debug)]
pub struct ExprBinary<'p> {
    pub left: Box<Expr<'p>>,
    pub op: BinOp<'p>,
    pub right: Box<Expr<'p>>,
}

impl<'p> ToTokens<'p> for ExprBinary<'p> {
    fn to_tokens(&self) -> TokenStream<'p, 'p> {
        self.left
            .to_tokens()
            .into_iter()
            .chain(self.op.to_tokens())
            .chain(self.right.to_tokens())
            .collect()
    }
}

/// A binary operation
#[derive(Debug)]
pub enum BinOp<'p> {
    Add(Plus<'p>),
    Sub(Sub<'p>),
    Mult(Mult<'p>),
    Div(Div<'p>),
    Rem(Rem<'p>),
}

impl<'p> Parse<'p> for BinOp<'p> {
    type Err = Error<'p>;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, Self::Err> {
        let tok = parser
            .peek()?
            .ok_or::<Error>(ErrorKind::UnexpectedEof.into())?;
        match tok.kind() {
            TokenKind::Plus => Ok(Self::Add(parser.parse()?)),
            TokenKind::Minus => Ok(Self::Sub(parser.parse()?)),
            TokenKind::Star => Ok(Self::Mult(parser.parse()?)),
            TokenKind::Div => Ok(Self::Div(parser.parse()?)),
            TokenKind::Rem => Ok(Self::Rem(parser.parse()?)),
            _ => {
                let kind = ErrorKind::expected_token(["+", "-", "/", "*", "%"], parser.consume()?);
                Err(parser.error(kind, None))
            }
        }
    }
}

impl<'p> ToTokens<'p> for BinOp<'p> {
    fn to_tokens(&self) -> TokenStream<'p, 'p> {
        match self {
            BinOp::Add(a) => a.to_tokens(),
            BinOp::Sub(a) => a.to_tokens(),
            BinOp::Mult(a) => a.to_tokens(),
            BinOp::Div(a) => a.to_tokens(),
            BinOp::Rem(a) => a.to_tokens(),
        }
    }
}

/// A call expression
#[derive(Debug)]
pub struct CallExpr<'p> {
    pub callee: Box<Expr<'p>>,
    pub lparen: LParen<'p>,
    pub parameters: Punctuated<Expr<'p>, Comma<'p>>,
    pub rparen: RParen<'p>,
}

impl<'p> ToTokens<'p> for CallExpr<'p> {
    fn to_tokens(&self) -> TokenStream<'p, 'p> {
        self.callee
            .to_tokens()
            .chain(self.lparen.to_tokens())
            .chain(self.parameters.to_tokens())
            .chain(self.rparen.to_tokens())
            .collect()
    }
}

/// A field expression
#[derive(Debug)]
pub struct FieldExpr<'p> {
    pub obj: Box<Expr<'p>>,
    pub dot: Dot<'p>,
    pub field: Id<'p>,
}
