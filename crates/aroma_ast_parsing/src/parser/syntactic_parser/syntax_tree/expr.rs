use crate::parser::singletons::*;
use crate::parser::{Constant, CouldParse, Error, ErrorKind, Parse, Punctuated0, Punctuated1, SyntacticParser};
use aroma_ast::id::Id;
use aroma_ast::token::{ToTokens, TokenKind, TokenStream};
use std::io::Read;

/// Parses an expression
#[inline]
pub fn parse_expr<'p>(parser: &mut SyntacticParser<'p, impl Read>) -> Result<Expr<'p>, Error<'p>> {
    let peek = parser.peek()?.map(|t| t.kind()).cloned();
    match peek.ok_or_else(|| parser.error(ErrorKind::UnexpectedEof, None))? {
        TokenKind::LCurly => {
            parse_closure(parser)
        }
        _ => {
            parse_or(parser)
        }
    }
}

fn parse_closure<'p>(parser: &mut SyntacticParser<'p, impl Read>) -> Result<Expr<'p>, Error<'p>> {
    let lcurl = parser.parse::<LCurly>()?;

    let parameters = parser.parse::<Punctuated0<Id<'p>, Comma<'p>>>()?;
    let arrow = parser.parse::<Arrow>()?;

    let rcurl = parser.parse::<RCurly>()?;

    Ok(Expr::Closure(ClosureExpr {
        lcurl,
        parameters,
        arrow,
        expressions: Default::default(),
        rcurl,
    }))
}

fn parse_or<'p>(parser: &mut SyntacticParser<'p, impl Read>) -> Result<Expr<'p>, Error<'p>> {
    let mut l = parse_and(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::Or)) {
        let binop = parser.parse::<BinOp>()?;
        let r = parse_and(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}

fn parse_and<'p>(parser: &mut SyntacticParser<'p, impl Read>) -> Result<Expr<'p>, Error<'p>> {
    let mut l = parse_cmp(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::And)) {
        let binop = parser.parse::<BinOp>()?;
        let r = parse_cmp(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}
fn parse_cmp<'p>(parser: &mut SyntacticParser<'p, impl Read>) -> Result<Expr<'p>, Error<'p>> {
    let mut l = parse_bitwise_or(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::Eq | TokenKind::Neq | TokenKind::Lt | TokenKind::Lte | TokenKind::Gt | TokenKind::Gte))
    {
        let binop = parser.parse::<BinOp>()?;
        let r = parse_bitwise_or(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}

fn parse_bitwise_or<'p>(
    parser: &mut SyntacticParser<'p, impl Read>,
) -> Result<Expr<'p>, Error<'p>> {
    let mut l = parse_bitwise_xor(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::BitwiseOr)) {
        let binop = parser.parse::<BinOp>()?;
        let r = parse_bitwise_xor(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}
fn parse_bitwise_xor<'p>(
    parser: &mut SyntacticParser<'p, impl Read>,
) -> Result<Expr<'p>, Error<'p>> {
    let mut l = parse_bitwise_and(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::BitwiseXor)) {
        let binop = parser.parse::<BinOp>()?;
        let r = parse_bitwise_and(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}
fn parse_bitwise_and<'p>(
    parser: &mut SyntacticParser<'p, impl Read>,
) -> Result<Expr<'p>, Error<'p>> {
    let mut l = parse_shift(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::BitwiseAnd)) {
        let binop = parser.parse::<BinOp>()?;
        let r = parse_shift(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}
fn parse_shift<'p>(parser: &mut SyntacticParser<'p, impl Read>) -> Result<Expr<'p>, Error<'p>> {
    let mut l = parse_add(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::LShift | TokenKind::RShift))
    {
        let binop = parser.parse::<BinOp>()?;
        let r = parse_add(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}

fn parse_add<'p>(parser: &mut SyntacticParser<'p, impl Read>) -> Result<Expr<'p>, Error<'p>> {
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
    let mut l = parse_unary(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::Star | TokenKind::Div))
    {
        let binop = parser.parse::<BinOp>()?;
        let r = parse_unary(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}

fn parse_unary<'p>(parser: &mut SyntacticParser<'p, impl Read>) -> Result<Expr<'p>, Error<'p>> {
    if matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::Minus | TokenKind::Bang)) {
        let unary_op = parser.parse::<UnaryOp>()?;
        let unary = parse_unary(parser)?;
        Ok(Expr::Unary(ExprUnary {
            op: unary_op,
            expr: Box::new(unary),
        }))
    } else {
        parse_tail(parser)
    }
}

fn parse_tail<'p>(parser: &mut SyntacticParser<'p, impl Read>) -> Result<Expr<'p>, Error<'p>> {
    let mut owner = parse_primary(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::LParen | TokenKind::LBracket | TokenKind::LCurly | TokenKind::Dot))
    {
        let token = parser.peek()?.unwrap();
        match token.kind() {
            TokenKind::LParen | TokenKind::LCurly => {
                owner = parse_call(owner, parser)?;
            }
            TokenKind::LBracket => {
                owner = parse_index(owner, parser)?;
            }
            TokenKind::Dot => {
                owner = parse_field(owner, parser)?;
            }
            _ => {
                unreachable!()
            }
        }
    }

    Ok(owner)
}

fn parse_call<'p>(
    mut owner: Expr<'p>,
    parser: &mut SyntacticParser<'p, impl Read>,
) -> Result<Expr<'p>, Error<'p>> {
    while LParen::could_parse(parser)? {
        let lparen = parser.parse::<LParen>()?;
        let punctuated = if RParen::could_parse(parser)? {
            None
        } else {
            Some(parser.parse::<Punctuated1<_, _>>()?)
        };
        let rparen = parser.parse::<RParen>()?;
        owner = Expr::Call(CallExpr {
            callee: Box::new(owner),
            lparen,
            parameters: punctuated,
            rparen,
        });
    }
    if LCurly::could_parse(parser)? {
        let cls = parse_closure(parser)?;
        owner = match owner {
            Expr::Call(mut call_expr) => {
                let mut parameters = call_expr.parameters.take().unwrap_or_default();
                parameters.punctuated.push((cls, None));
                Expr::Call(
                    CallExpr {
                        parameters: Some(parameters),
                        ..call_expr
                    }
                )
            }
            other => {
                let mut parameters = Punctuated1::default();
                parameters.punctuated.push((cls, None));
                Expr::Call(
                    CallExpr {
                        callee: Box::new(other),
                        lparen: LParen::new(),
                        parameters: Some(parameters),
                        rparen: RParen::new(),
                    }
                )
            }
        }
    }

    Ok(owner)
}

fn parse_field<'p>(
    mut owner: Expr<'p>,
    parser: &mut SyntacticParser<'p, impl Read>,
) -> Result<Expr<'p>, Error<'p>> {
    while Dot::could_parse(parser)? {
        let dot = parser.parse::<Dot>()?;
        let Some(field) = parser.consume_if(|p| matches!(p.kind(), TokenKind::Identifier(_)))?
        else {
            return Err(parser.error(ErrorKind::expected_token(["identifier"], None), None));
        };
        let field = Id::new([field]).unwrap();
        owner = Expr::Field(FieldExpr {
            obj: Box::new(owner),
            dot,
            field,
        });
    }
    Ok(owner)
}

fn parse_index<'p>(
    mut owner: Expr<'p>,
    parser: &mut SyntacticParser<'p, impl Read>,
) -> Result<Expr<'p>, Error<'p>> {
    while LBracket::could_parse(parser)? {
        let lbracket = parser.parse::<LBracket>()?;
        let index = parser.parse::<Expr>()?;
        let rbracket = parser.parse::<RBracket>()?;
        owner = Expr::Index(IndexExpr {
            obj: Box::new(owner),
            lbracket,
            index: Box::new(index),
            rbracket,
        });
    }
    Ok(owner)
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
            let lparen = parser.parse::<LParen>()?;
            let expr = parser.parse::<Expr>()?;
            let rparen = parser.parse::<RParen>()?;
            Ok(Expr::Nested(NestedExpr {
                lparen,
                expr: Box::new(expr),
                rparen,
            }))
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
#[derive(Debug, ToTokens)]
pub struct ExprUnary<'p> {
    pub op: UnaryOp<'p>,
    pub expr: Box<Expr<'p>>,
}
/// A unary operation
#[derive(Debug, ToTokens)]
pub enum UnaryOp<'p> {
    Sub(Sub<'p>),
    Not(Not<'p>),
}

impl<'p> Parse<'p> for UnaryOp<'p> {
    type Err = Error<'p>;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, Self::Err> {
        let tok = parser
            .peek()?
            .ok_or::<Error>(ErrorKind::UnexpectedEof.into())?;
        match tok.kind() {
            TokenKind::Minus => Ok(Self::Sub(parser.parse()?)),
            TokenKind::Bang => Ok(Self::Not(parser.parse()?)),
            _ => {
                let kind = ErrorKind::expected_token(["-", "!"], parser.consume()?);
                Err(parser.error(kind, None))
            }
        }
    }
}

#[derive(Debug, ToTokens)]
pub struct ExprBinary<'p> {
    pub left: Box<Expr<'p>>,
    pub op: BinOp<'p>,
    pub right: Box<Expr<'p>>,
}

/// A binary operation
#[derive(Debug, ToTokens)]
pub enum BinOp<'p> {
    Mult(Mult<'p>),
    Div(Div<'p>),
    Rem(Rem<'p>),
    Add(Plus<'p>),
    Sub(Sub<'p>),
    LShift(LShift<'p>),
    RShift(RShift<'p>),
    BitwiseAnd(BitwiseAnd<'p>),
    BitwiseOr(BitwiseOr<'p>),
    BitwiseXor(BitwiseXor<'p>),
    Eq(Eq<'p>),
    Neq(Neq<'p>),
    Lt(Lt<'p>),
    Lte(Lte<'p>),
    Gt(Gt<'p>),
    Gte(Gte<'p>),
    And(And<'p>),
    Or(Or<'p>),
}

impl<'p> BinOp<'p> {
    fn lookahead_set() -> &'static [TokenKind] {
        &[
            TokenKind::Star,
            TokenKind::Div,
            TokenKind::Rem,
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::LShift,
            TokenKind::RShift,
            TokenKind::BitwiseAnd,
            TokenKind::BitwiseOr,
            TokenKind::BitwiseXor,
            TokenKind::Eq,
            TokenKind::Neq,
            TokenKind::Lt,
            TokenKind::Lte,
            TokenKind::Gt,
            TokenKind::Gte,
            TokenKind::And,
            TokenKind::Or,
        ]
    }
}

impl<'p> Parse<'p> for BinOp<'p> {
    type Err = Error<'p>;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, Self::Err> {
        let tok = parser
            .peek()?
            .ok_or::<Error>(ErrorKind::UnexpectedEof.into())?;
        match tok.kind() {
            TokenKind::Star => Ok(Self::Mult(parser.parse()?)),
            TokenKind::Div => Ok(Self::Div(parser.parse()?)),
            TokenKind::Rem => Ok(Self::Rem(parser.parse()?)),
            TokenKind::Plus => Ok(Self::Add(parser.parse()?)),
            TokenKind::Minus => Ok(Self::Sub(parser.parse()?)),

            TokenKind::LShift => Ok(Self::LShift(parser.parse()?)),
            TokenKind::RShift => Ok(Self::RShift(parser.parse()?)),

            TokenKind::BitwiseAnd => Ok(Self::BitwiseAnd(parser.parse()?)),
            TokenKind::BitwiseOr => Ok(Self::BitwiseOr(parser.parse()?)),
            TokenKind::BitwiseXor => Ok(Self::BitwiseXor(parser.parse()?)),
            TokenKind::Eq => Ok(Self::Eq(parser.parse()?)),
            TokenKind::Neq => Ok(Self::Neq(parser.parse()?)),
            TokenKind::Lt => Ok(Self::Lt(parser.parse()?)),
            TokenKind::Lte => Ok(Self::Lte(parser.parse()?)),
            TokenKind::Gt => Ok(Self::Gt(parser.parse()?)),
            TokenKind::Gte => Ok(Self::Gte(parser.parse()?)),
            TokenKind::And => Ok(Self::And(parser.parse()?)),
            TokenKind::Or => Ok(Self::Or(parser.parse()?)),
            _ => {
                let kind = ErrorKind::expected_token(
                    [
                        "+", "-", "/", "*", "%", ">>", "<<", "&", "|", "^", "==", "!=", "<", "<=",
                        ">", ">=", "&&", "||",
                    ],
                    parser.consume()?,
                );
                Err(parser.error(kind, None))
            }
        }
    }
}

/// A call expression
#[derive(Debug, ToTokens)]
pub struct CallExpr<'p> {
    pub callee: Box<Expr<'p>>,
    pub lparen: LParen<'p>,
    pub parameters: Option<Punctuated1<Expr<'p>, Comma<'p>>>,
    pub rparen: RParen<'p>,
}

/// A field expression
#[derive(Debug, ToTokens)]
pub struct FieldExpr<'p> {
    pub obj: Box<Expr<'p>>,
    pub dot: Dot<'p>,
    pub field: Id<'p>,
}

/// A field expression
#[derive(Debug, ToTokens)]
pub struct IndexExpr<'p> {
    pub obj: Box<Expr<'p>>,
    pub lbracket: LBracket<'p>,
    pub index: Box<Expr<'p>>,
    pub rbracket: RBracket<'p>,
}

/// A closure expression
#[derive(Debug, ToTokens)]
pub struct ClosureExpr<'p> {
    pub lcurl: LCurly<'p>,
    pub parameters: Punctuated0<Id<'p>, Comma<'p>>,
    pub arrow: Arrow<'p>,
    pub expressions: Punctuated0<Expr<'p>, SemiC<'p>>,
    pub rcurl: RCurly<'p>,
}
/// A closure expression
#[derive(Debug, ToTokens)]
pub struct NestedExpr<'p> {
    pub lparen: LParen<'p>,
    pub expr: Box<Expr<'p>>,
    pub rparen: RParen<'p>,
}

/// An expression
#[derive(Debug, ToTokens)]
pub enum Expr<'p> {
    Path(Id<'p>),
    Constant(Constant<'p>),
    Binary(ExprBinary<'p>),
    Field(FieldExpr<'p>),
    Call(CallExpr<'p>),
    Index(IndexExpr<'p>),
    Unary(ExprUnary<'p>),
    Closure(ClosureExpr<'p>),
    Nested(NestedExpr<'p>),
}

impl<'p> Parse<'p> for Expr<'p> {
    type Err = Error<'p>;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, Self::Err> {
        parse_expr(parser)
    }
}
