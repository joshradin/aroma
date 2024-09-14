use crate::parser::hir::constants::Constant;
use crate::parser::hir::singletons::*;
use crate::parser::hir::statement::StatementList;
use crate::parser::hir::{Err, ErrorKind, Punctuated0, Punctuated1, SyntaxError};
use crate::parser::SyntaxResult;
use aroma_tokens::id::Id;
use aroma_tokens::token::{ToTokens, TokenKind};
use std::io::Read;
use crate::parser::blocking::{remove_nl, BlockingParser};
use crate::parser::hir_parser::blocking::{CouldParse, Parsable};

#[derive(Debug, ToTokens)]
pub struct ExprUnary {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}
/// A unary operation
#[derive(Debug, ToTokens)]
pub enum UnaryOp {
    Sub(Sub),
    Not(Not),
}

impl Parsable for UnaryOp {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
        let tok = parser
            .peek()?
            .ok_or::<SyntaxError>(ErrorKind::UnexpectedEof.into())?;
        match tok.kind() {
            TokenKind::Minus => Ok(Self::Sub(parser.parse(Sub::parse)?)),
            TokenKind::Bang => Ok(Self::Not(parser.parse(Not::parse)?)),
            _ => {
                let kind = ErrorKind::expected_token(["-", "!"], parser.consume()?);
                Err(parser.error(kind, None))
            }
        }
    }
}

#[derive(Debug, ToTokens)]
pub struct ExprBinary {
    pub left: Box<Expr>,
    pub op: BinOp,
    pub right: Box<Expr>,
}

/// A binary operation
#[derive(Debug, ToTokens)]
pub enum BinOp {
    Mult(Mult),
    Div(Div),
    Rem(Rem),
    Add(Plus),
    Sub(Sub),
    LShift(LShift),
    RShift(RShift),
    BitwiseAnd(BitwiseAnd),
    BitwiseOr(BitwiseOr),
    BitwiseXor(BitwiseXor),
    Eq(Eq),
    Neq(Neq),
    Lt(Lt),
    Lte(Lte),
    Gt(Gt),
    Gte(Gte),
    And(And),
    Or(Or),
}

impl BinOp {
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

impl Parsable for BinOp {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
        let tok = parser
            .peek()?
            .ok_or::<SyntaxError>(ErrorKind::UnexpectedEof.into())?;
        match tok.kind() {
            TokenKind::Star => Ok(Self::Mult(parser.parse(Mult::parse)?)),
            TokenKind::Div => Ok(Self::Div(parser.parse(Div::parse)?)),
            TokenKind::Rem => Ok(Self::Rem(parser.parse(Rem::parse)?)),
            TokenKind::Plus => Ok(Self::Add(parser.parse(Plus::parse)?)),
            TokenKind::Minus => Ok(Self::Sub(parser.parse(Sub::parse)?)),

            TokenKind::LShift => Ok(Self::LShift(parser.parse(LShift::parse)?)),
            TokenKind::RShift => Ok(Self::RShift(parser.parse(RShift::parse)?)),

            TokenKind::BitwiseAnd => Ok(Self::BitwiseAnd(parser.parse(BitwiseAnd::parse)?)),
            TokenKind::BitwiseOr => Ok(Self::BitwiseOr(parser.parse(BitwiseOr::parse)?)),
            TokenKind::BitwiseXor => Ok(Self::BitwiseXor(parser.parse(BitwiseXor::parse)?)),
            TokenKind::Eq => Ok(Self::Eq(parser.parse(Eq::parse)?)),
            TokenKind::Neq => Ok(Self::Neq(parser.parse(Neq::parse)?)),
            TokenKind::Lt => Ok(Self::Lt(parser.parse(Lt::parse)?)),
            TokenKind::Lte => Ok(Self::Lte(parser.parse(Lte::parse)?)),
            TokenKind::Gt => Ok(Self::Gt(parser.parse(Gt::parse)?)),
            TokenKind::Gte => Ok(Self::Gte(parser.parse(Gte::parse)?)),
            TokenKind::And => Ok(Self::And(parser.parse(And::parse)?)),
            TokenKind::Or => Ok(Self::Or(parser.parse(Or::parse)?)),
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
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub lparen: LParen,
    pub parameters: Option<Punctuated1<Expr, Comma>>,
    pub rparen: RParen,
}

/// A field expression
#[derive(Debug, ToTokens)]
pub struct FieldExpr {
    pub obj: Box<Expr>,
    pub dot: Dot,
    pub field: Id,
}

/// A field expression
#[derive(Debug, ToTokens)]
pub struct IndexExpr {
    pub obj: Box<Expr>,
    pub lbracket: LBracket,
    pub index: Box<Expr>,
    pub rbracket: RBracket,
}

/// A closure expression
#[derive(Debug, ToTokens)]
pub struct ClosureExpr {
    pub lcurl: LCurly,
    pub parameters: Option<Punctuated0<Id, Comma>>,
    pub arrow: Option<Arrow>,
    pub list: StatementList,
    pub rcurl: RCurly,
}
/// A closure expression
#[derive(Debug, ToTokens)]
pub struct NestedExpr {
    pub lparen: LParen,
    pub expr: Box<Expr>,
    pub rparen: RParen,
}

/// A ternary expression
#[derive(Debug, ToTokens)]
pub struct TernaryExpr {
    pub cond: Box<Expr>,
    pub qmark: QMark,
    pub then_expr: Box<Expr>,
    pub colon: Colon,
    pub else_expr: Box<Expr>,
}

/// A list expression
#[derive(Debug, ToTokens)]
pub struct ListExpr {
    pub lbracket: LBracket,
    pub items: Option<Punctuated1<Expr, Comma>>,
    pub rbracket: RBracket,
}

/// A list expression
#[derive(Debug, ToTokens)]
pub struct MapExpr {
    pub lbracket: LBracket,
    pub items: Option<Punctuated1<Expr, Comma>>,
    pub rbracket: RBracket,
}

/// This expr
#[derive(Debug, ToTokens)]
pub struct ThisExpr {
    pub this: This,
}

/// Super expr
#[derive(Debug, ToTokens)]
pub struct SuperExpr {
    pub super_tok: Super,
}

/// Super expr
#[derive(Debug, ToTokens)]
pub struct DelegateExpr {
    pub delegate: Delegate,
}

/// An expression
#[derive(Debug, ToTokens)]
pub enum Expr {
    Path(Id),
    List(ListExpr),
    Constant(Constant),
    Binary(ExprBinary),
    Field(FieldExpr),
    Call(CallExpr),
    Index(IndexExpr),
    Unary(ExprUnary),
    Closure(ClosureExpr),
    Nested(NestedExpr),
    Ternary(TernaryExpr),
    This(ThisExpr),
    Super(SuperExpr),
    Delegate(DelegateExpr),
}

impl Parsable for Expr {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
        parse_expr(parser)
    }
}
/// Parses an expression
#[inline]
pub fn parse_expr(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
    parser.parse(remove_nl)?;
    let peek = parser.peek()?.map(|t| t.kind()).cloned();
    match peek.ok_or_else(|| parser.error(ErrorKind::UnexpectedEof, None))? {
        TokenKind::LCurly => parse_closure(parser),
        _ => {
            let e = parse_or(parser)?;
            if QMark::could_parse(parser)? {
                let qmark = parser.parse(QMark::parse)?;
                parser.parse(remove_nl)?;
                let then_expr = parse_or(parser)?;
                let colon = parser.parse(Colon::parse)?;
                let else_expr = parse_expr(parser)?;
                Ok(Expr::Ternary(TernaryExpr {
                    cond: Box::new(e),
                    qmark,
                    then_expr: Box::new(then_expr),
                    colon,
                    else_expr: Box::new(else_expr),
                }))
            } else {
                Ok(e)
            }
        }
    }
}

fn parse_closure(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
    let lcurl = parser.parse(LCurly::parse)?;
    let (opt_parameters, opt_arrow) = parser
        .try_parse(|parser: &mut BlockingParser<_>| {
            let parameters = parser.parse(Punctuated0::<Id, Comma>::parse)?;
            let arrow = parser.parse(Arrow::parse)?;
            Ok((parameters, arrow))
        })?
        .unzip();
    let list = parser.parse(StatementList::parse)?;
    let rcurl = parser.parse(RCurly::parse)?;

    Ok(Expr::Closure(ClosureExpr {
        lcurl,
        parameters: opt_parameters,
        arrow: opt_arrow,
        list,
        rcurl,
    }))
}

fn parse_or(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
    let mut l = parse_and(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::Or)) {
        let binop = parser.parse(BinOp::parse)?;
        let r = parse_and(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}

fn parse_and(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
    let mut l = parse_cmp(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::And)) {
        let binop = parser.parse(BinOp::parse)?;
        let r = parse_cmp(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}
fn parse_cmp(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
    let mut l = parse_bitwise_or(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::Eq | TokenKind::Neq | TokenKind::Lt | TokenKind::Lte | TokenKind::Gt | TokenKind::Gte))
    {
        let binop = parser.parse(BinOp::parse)?;
        let r = parse_bitwise_or(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}

fn parse_bitwise_or(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
    let mut l = parse_bitwise_xor(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::BitwiseOr)) {
        let binop = parser.parse(BinOp::parse)?;
        let r = parse_bitwise_xor(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}
fn parse_bitwise_xor(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
    let mut l = parse_bitwise_and(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::BitwiseXor)) {
        let binop = parser.parse(BinOp::parse)?;
        let r = parse_bitwise_and(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}
fn parse_bitwise_and(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
    let mut l = parse_shift(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::BitwiseAnd)) {
        let binop = parser.parse(BinOp::parse)?;
        let r = parse_shift(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}
fn parse_shift(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
    let mut l = parse_add(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::LShift | TokenKind::RShift))
    {
        let binop = parser.parse(BinOp::parse)?;
        let r = parse_add(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}

fn parse_add(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
    let mut l = parse_group(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::Plus | TokenKind::Minus))
    {
        let binop = parser.parse(BinOp::parse)?;
        let r = parse_group(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}

fn parse_group(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
    let mut l = parse_unary(parser)?;
    while matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::Star | TokenKind::Div))
    {
        let binop = parser.parse(BinOp::parse)?;
        let r = parse_unary(parser)?;
        l = Expr::Binary(ExprBinary {
            left: Box::new(l),
            op: binop,
            right: Box::new(r),
        })
    }
    Ok(l)
}

fn parse_unary(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
    if matches!(parser.peek()?, Some(t) if matches!(t.kind(), TokenKind::Minus | TokenKind::Bang)) {
        let unary_op = parser.parse(UnaryOp::parse)?;
        let unary = parse_unary(parser)?;
        Ok(Expr::Unary(ExprUnary {
            op: unary_op,
            expr: Box::new(unary),
        }))
    } else {
        parse_tail(parser)
    }
}

fn parse_tail(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
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

fn parse_call(
    mut owner: Expr,
    parser: &mut BlockingParser<impl Read>,
) -> Result<Expr, Err<SyntaxError>> {
    while LParen::could_parse(parser)? {
        let lparen = parser.parse(LParen::parse)?;
        let punctuated = if RParen::could_parse(parser)? {
            None
        } else {
            Some(parser.parse(Punctuated1::<_, _>::parse)?)
        };
        let rparen = parser.parse(RParen::parse)?;
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
                Expr::Call(CallExpr {
                    parameters: Some(parameters),
                    ..call_expr
                })
            }
            other => {
                let mut parameters = Punctuated1::default();
                parameters.punctuated.push((cls, None));
                Expr::Call(CallExpr {
                    callee: Box::new(other),
                    lparen: LParen::new(),
                    parameters: Some(parameters),
                    rparen: RParen::new(),
                })
            }
        }
    }

    Ok(owner)
}

fn parse_field(
    mut owner: Expr,
    parser: &mut BlockingParser<impl Read>,
) -> Result<Expr, Err<SyntaxError>> {
    while Dot::could_parse(parser)? {
        let dot = parser.parse(Dot::parse)?;
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

fn parse_index(
    mut owner: Expr,
    parser: &mut BlockingParser<impl Read>,
) -> Result<Expr, Err<SyntaxError>> {
    while LBracket::could_parse(parser)? {
        let lbracket = parser.parse(LBracket::parse)?;
        let index = parser.parse(Expr::parse)?;
        let rbracket = parser.parse(RBracket::parse)?;
        owner = Expr::Index(IndexExpr {
            obj: Box::new(owner),
            lbracket,
            index: Box::new(index),
            rbracket,
        });
    }
    Ok(owner)
}

fn parse_primary(parser: &mut BlockingParser<impl Read>) -> Result<Expr, Err<SyntaxError>> {
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
            let constant = parser.parse(Constant::parse)?;
            Ok(Expr::Constant(constant))
        }
        Some(t) if matches!(t.kind(), TokenKind::Identifier(_)) => {
            let id = parser.parse(Id::parse)?;
            Ok(Expr::Path(id))
        }
        Some(t) if matches!(t.kind(), TokenKind::LParen) => {
            let lparen = parser.parse(LParen::parse)?;
            let expr = parser.parse(Expr::parse)?;
            let rparen = parser.parse(RParen::parse)?;
            Ok(Expr::Nested(NestedExpr {
                lparen,
                expr: Box::new(expr),
                rparen,
            }))
        }
        Some(t) if matches!(t.kind(), TokenKind::LBracket) => {
            let lbracket = parser.parse(LBracket::parse)?;
            let exprs = if RBracket::could_parse(parser)? {
                None
            } else {
                Some(parser.parse(Punctuated1::parse)?)
            };
            let rbracket = parser.parse(RBracket::parse)?;
            Ok(Expr::List(ListExpr {
                lbracket,
                items: exprs,
                rbracket,
            }))
        }
        Some(t) if matches!(t.kind(), TokenKind::This) => {
            let this = parser.parse(This::parse)?;
            Ok(Expr::This(ThisExpr { this }))
        }
        Some(t) if matches!(t.kind(), TokenKind::Super) => {
            let super_tok = parser.parse(Super::parse)?;
            Ok(Expr::Super(SuperExpr { super_tok }))
        }
        Some(t) if matches!(t.kind(), TokenKind::This) => {
            let delegate = parser.parse(Delegate::parse)?;
            Ok(Expr::Delegate(DelegateExpr { delegate }))
        }
        _ => {
            let kind = ErrorKind::expected_token(
                [
                    "constant".to_string(),
                    "identifier".to_string(),
                    "this".to_string(),
                    "super".to_string(),
                    "delegate".to_string(),
                    "(".to_string(),
                    "[".to_string(),
                ],
                parser.consume()?,
            );
            Err(parser.error(kind, None))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::hir_parser::tests::test_parser;
    use crate::parser::hir_parser::blocking::Parsable;
    use aroma_tokens::id::Id;
    use aroma_tokens::token::ToTokens;
    use crate::parser::hir::expr::Expr;

    #[test]
    fn test_parse_identifier() {
        test_parser("helloWorld", |parser, _| {
            let identifier = parser.parse(Id::parse).unwrap();
            assert!(matches!(
                identifier.most_specific().try_as_ref().unwrap(),
                "helloWorld"
            ));
        })
    }

    #[test]
    fn test_parse_identifier_single_letter() {
        test_parser("i", |parser, _| {
            let identifier = parser.parse(Id::parse).unwrap();
            assert!(matches!(
                identifier.most_specific().try_as_ref().unwrap(),
                "i"
            ));
        })
    }

    #[test]
    fn test_parse_unary() {
        test_parser("--helloWorld", |parser, _| {
            let expr = parser.parse(Expr::parse).unwrap();
            println!("{expr:#?}");
        })
    }

    #[test]
    fn test_parse_expressions() {
        test_parser("helloWorld + 3*2/5 - 1 << 3 * -2.0", |parser, _| {
            let expr = parser.parse(Expr::parse).unwrap();
            println!("{expr:#?}");
            println!("{:?}", expr.to_tokens().collect::<Vec<_>>())
        })
    }

    #[test]
    fn test_parse_method_calls() {
        test_parser("foo(bar() + 2, bar(1+2))", |parser, _| {
            let expr = parser.parse(Expr::parse).unwrap();
            println!("{expr:#?}");
            println!("{:?}", expr.to_tokens().collect::<Vec<_>>())
        })
    }

    #[test]
    fn test_parse_fields() {
        test_parser("foo.blah.get(1)", |parser, _| {
            let expr = parser.parse(Expr::parse).unwrap();
            println!("{expr:#?}");
            println!("{:?}", expr.to_tokens().collect::<Vec<_>>())
        })
    }

    #[test]
    fn test_parse_index() {
        test_parser("foo[0]", |parser, _| {
            let expr = parser.parse(Expr::parse).unwrap();
            println!("{expr:#?}");
            println!("{:?}", expr.to_tokens().collect::<Vec<_>>());
        })
    }

    #[test]
    fn test_closure() {
        test_parser("{ -> }", |parser, _| {
            let expr = parser.parse(Expr::parse).unwrap();
            println!("{expr:#?}");
            println!("{:?}", expr.to_tokens().collect::<Vec<_>>());
        })
    }

    #[test]
    fn test_closure_with_bindings() {
        test_parser("{ i -> }", |parser, _| {
            let expr = parser.parse(Expr::parse).unwrap();
            println!("{expr:#?}");
            println!("{:?}", expr.to_tokens().collect::<Vec<_>>());
        })
    }

    #[test]
    fn test_closure_after_method_no_parens() {
        test_parser(
            r"[1, 2, 3].forEach { it ->
            let a = 2
         }",
            |parser, _| {
                let expr = parser.parse(Expr::parse).unwrap_or_else(|e| panic!("{e}"));
                println!("{expr:#?}");
                println!("{:#?}", expr.to_token_tree());
            },
        )
    }

    #[test]
    fn test_closure_after_method_no_args() {
        test_parser("forEach() { -> }", |parser, _| {
            let expr = parser.parse(Expr::parse).unwrap();
            println!("{expr:#?}");
            println!("{:#?}", expr.to_token_tree());
            assert!(
                matches!(expr, Expr::Call(call) if call.parameters.is_some() && call.parameters.as_ref().unwrap().punctuated.len() == 1)
            );
        })
    }

    #[test]
    fn test_closure_after_method_no_args_no_arrow() {
        test_parser("forEach(){ }", |parser, _| {
            let expr = parser.parse(Expr::parse).unwrap();
            println!("{expr:#?}");
            assert!(
                matches!(expr, Expr::Call(call) if call.parameters.is_some() && call.parameters.as_ref().unwrap().punctuated.len() == 1)
            );
        })
    }

    #[test]
    fn test_closure_after_method_mixed_args() {
        test_parser("repeat(10) {i -> }", |parser, _| {
            let expr = parser.parse(Expr::parse).unwrap();
            println!("{expr:#?}");
            assert!(
                matches!(expr, Expr::Call(call) if call.parameters.is_some() && call.parameters.as_ref().unwrap().punctuated.len() == 2)
            );
        })
    }

    #[test]
    fn test_closure_after_method_mixed_args_then_expr() {
        test_parser("(repeat(10)) { -> } ", |parser, _| {
            let expr = parser.parse(Expr::parse).unwrap();
            println!("{expr:#?}");
            assert!(
                matches!(expr, Expr::Call(call) if call.parameters.is_some() && call.parameters.as_ref().unwrap().punctuated.len() == 1)
            );
        })
    }

    #[test]
    fn test_ternary() {
        test_parser("a < 3 ? 1 : 3", |parser, _| {
            let expr = parser.parse(Expr::parse).unwrap();
            println!("{expr:#?}");
            assert!(matches!(expr, Expr::Ternary(_)));
        })
    }

    #[test]
    fn test_multiline() {
        test_parser(
            r"a < 3 ?
                1 :
                3",
            |parser, _| {
                parser.set_ignore_nl(false).expect("could not set ignore");
                let expr = parser.parse(Expr::parse).unwrap();
                println!("{expr:#?}");
                assert!(matches!(expr, Expr::Ternary(_)));
            },
        )
    }
}
