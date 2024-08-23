//! statements

use crate::parser::binding::OptTypeBinding;
use crate::parser::expr::{remove_nl, Expr};
use crate::parser::singletons::*;
use crate::parser::syntactic_parser::hir::helpers::End;
use crate::parser::{cut, multi1, CouldParse, ErrorKind, Parsable, SyntacticParser, SyntaxError};
use aroma_ast::token::{ToTokens, TokenKind};
use std::io::Read;

/// General statement types
#[derive(Debug, ToTokens)]
pub enum Statement<'p> {
    Let(StatementLet<'p>),
    Const(StatementConst<'p>),
    Expr(StatementExpr<'p>),
    Block(StatementBlock<'p>),
    If(StatementIf<'p>),
    While(StatementWhile<'p>),
    Loop(StatementLoop<'p>),
    Return(StatementReturn<'p>),
    Break(StatementBreak<'p>),
    Continue(StatementContinue<'p>),
    Match(StatementMatch<'p>),
    TryCatch(StatementTryCatch<'p>),
}

impl Statement<'_> {
    /// If this is a binding statement or not.
    pub fn is_binding(&self) -> bool {
        matches!(self, Statement::Let(_) | Statement::Const(_))
    }

    /// Checks if this statement is terminated
    pub fn is_terminated(&self) -> bool {
        match self {
            Statement::Expr(expr) => expr.end.is_some(),
            _ => true,
        }
    }
}

impl<'p> Parsable<'p> for Statement<'p> {
    type Err = SyntaxError<'p>;

    fn parse<R: Read>(
        parser: &mut SyntacticParser<'p, R>,
    ) -> Result<Self, crate::parser::Err<Self::Err>> {
        parse_statement(parser)
    }
}
/// A list of statements
#[derive(Debug, ToTokens)]
pub struct StatementList<'p> {
    pub statements: Vec<Statement<'p>>,
}
impl<'p> Parsable<'p> for StatementList<'p> {
    type Err = SyntaxError<'p>;

    fn parse<R: Read>(
        parser: &mut SyntacticParser<'p, R>,
    ) -> Result<Self, crate::parser::Err<Self::Err>> {
        parse_statement_list(parser)
    }
}
/// A block of statements
#[derive(Debug, ToTokens)]
pub struct StatementBlock<'p> {
    pub lcurly: LCurly<'p>,
    pub list: StatementList<'p>,
    pub rcurly: RCurly<'p>,
}

impl<'p> Parsable<'p> for StatementBlock<'p> {
    type Err = SyntaxError<'p>;

    fn parse<R: Read>(
        parser: &mut SyntacticParser<'p, R>,
    ) -> Result<Self, crate::parser::Err<Self::Err>> {
        let lcurly = parser.parse(LCurly::parse)?;
        let list = parser.parse(StatementList::parse)?;
        let rcurly = parser.parse(RCurly::parse)?;
        Ok(StatementBlock {
            lcurly,
            list,
            rcurly,
        })
    }
}

#[derive(Debug, ToTokens)]
pub struct StatementLet<'p> {
    pub let_tok: Let<'p>,
    pub binding: OptTypeBinding<'p>,
    pub assign: Assign<'p>,
    pub expr: Expr<'p>,
    pub end: End<'p>,
}

#[derive(Debug, ToTokens)]
pub struct StatementConst<'p> {
    pub const_tok: Const<'p>,
    pub binding: OptTypeBinding<'p>,
    pub assign: Assign<'p>,
    pub expr: Expr<'p>,
    pub end: End<'p>,
}

/// If statement
#[derive(Debug, ToTokens)]
pub struct StatementIf<'p> {
    pub if_tok: If<'p>,
    pub lparen: LParen<'p>,
    pub condition: Expr<'p>,
    pub rparen: RParen<'p>,
    pub then_stmt: Box<Statement<'p>>,
    pub else_block: Option<ElseBlock<'p>>,
}

/// while statement
#[derive(Debug, ToTokens)]
pub struct StatementWhile<'p> {
    pub while_tok: While<'p>,
    pub lparen: LParen<'p>,
    pub condition: Expr<'p>,
    pub rparen: RParen<'p>,
    pub stmt: Box<Statement<'p>>,
}

/// for statement
#[derive(Debug, ToTokens)]
pub struct StatementFor<'p> {
    pub for_tok: For<'p>,
    pub lparen: LParen<'p>,
    pub condition: Expr<'p>,
    pub rparen: RParen<'p>,
    pub stmt: Box<Statement<'p>>,
}

/// for statement
#[derive(Debug, ToTokens)]
pub struct StatementLoop<'p> {
    pub loop_tok: Loop<'p>,
    pub block: Box<StatementBlock<'p>>,
}

#[derive(Debug, ToTokens)]
pub struct ElseBlock<'p> {
    pub else_tok: Else<'p>,
    pub else_stmt: Box<Statement<'p>>,
}

/// A statement consisting of a single expression that must ultimately be a function call.
#[derive(Debug, ToTokens)]
pub struct StatementExpr<'p> {
    pub expr: Expr<'p>,
    pub end: Option<End<'p>>,
}

#[derive(Debug, ToTokens)]
pub struct StatementReturn<'p> {
    pub ret: Return<'p>,
    pub expr: Option<Expr<'p>>,
    pub end: End<'p>,
}

#[derive(Debug, ToTokens)]
pub struct StatementBreak<'p> {
    pub break_tok: Break<'p>,
    pub end: End<'p>,
}

#[derive(Debug, ToTokens)]
pub struct StatementContinue<'p> {
    pub break_tok: Break<'p>,
    pub end: End<'p>,
}

#[derive(Debug, ToTokens)]
pub struct StatementMatch<'p> {
    pub match_tok: Match<'p>,
    pub lparen: LParen<'p>,
    pub condition: Expr<'p>,
    pub rparen: RParen<'p>,
}

#[derive(Debug, ToTokens)]
pub struct StatementTryCatch<'p> {
    pub try_tok: Try<'p>,
    pub statement_block: StatementBlock<'p>,
    pub catches: Vec<CatchBlock<'p>>,
}

#[derive(Debug, ToTokens)]
pub struct CatchBlock<'p> {
    pub catch: Catch<'p>,
    pub lparen: LParen<'p>,
    pub binding: OptTypeBinding<'p>,
    pub rparen: RParen<'p>,
    pub statement_block: StatementBlock<'p>,
}

/// Parse a statement block
fn parse_statement_list<'p, R: Read>(
    parser: &mut SyntacticParser<'p, R>,
) -> crate::parser::Result<'p, StatementList<'p>> {
    let mut list = vec![];
    parser.with_ignore_nl(false, |parser| {
        loop {
            if let Some(statement) = parser.try_parse(Statement::parse)? {
                list.push(statement);
            } else {
                break;
            }
        }
        Ok(())
    })?;

    let len = list.len();
    list.iter().enumerate().try_for_each(|(index, item)| {
        if index == len - 1 {
            return Ok(());
        }
        if let Statement::Expr(expr) = item {
            if !matches!(expr.expr, Expr::Call(_)) {
                return Err(parser.error(
                    ErrorKind::illegal_statement(
                        "expressions must be non function calls if not last statement",
                    ),
                    None,
                ));
            }
        }
        if !item.is_terminated() {
            return Err(parser.error(
                ErrorKind::illegal_statement(
                    "statements can only be non-terminated if last statement",
                ),
                None,
            ));
        }
        Ok(())
    })?;

    Ok(StatementList { statements: list })
}
/// Parse a statement block
fn parse_statement<'p, R: Read>(
    parser: &mut SyntacticParser<'p, R>,
) -> crate::parser::Result<'p, Statement<'p>> {
    parser.parse(remove_nl)?;
    let lookahead = parser
        .peek()?
        .cloned()
        .ok_or::<SyntaxError<'p>>(ErrorKind::UnexpectedEof.into())?;
    let statement: Statement<'p> = match lookahead.kind() {
        TokenKind::Let => {
            let let_tok = parser.parse(Let::parse)?;
            let binding = parser.parse(cut(OptTypeBinding::parse))?;
            let assign = parser.parse(cut(Assign::parse))?;
            let val = parser.parse(cut(Expr::parse))?;
            let end = parser.parse(cut(End::parse))?;

            Statement::Let(StatementLet {
                let_tok,
                binding,
                assign,
                expr: val,
                end,
            })
        }
        TokenKind::Const => {
            let const_tok = parser.parse(Const::parse)?;
            let binding = parser.parse(cut(OptTypeBinding::parse))?;
            let assign = parser.parse(cut(Assign::parse))?;
            let val = parser.parse(cut(Expr::parse))?;
            let end = parser.parse(cut(End::parse))?;

            Statement::Const(StatementConst {
                const_tok,
                binding,
                assign,
                expr: val,
                end,
            })
        }
        TokenKind::If => {
            let if_tok = parser.parse(If::parse)?;
            let lparen = parser.parse(LParen::parse)?;
            let condition = parser.parse(Expr::parse)?;
            let rparen = parser.parse(RParen::parse)?;

            let then_stmt = parser.parse(Statement::parse)?;
            if then_stmt.is_binding() {
                return Err(parser.error(
                    ErrorKind::illegal_statement("Can not having binding statement alone"),
                    None,
                ));
            }

            let else_block = parser.try_parse(|parser: &mut SyntacticParser<'p, R>| {
                let else_tok = parser.parse(Else::parse)?;
                let else_stmt = parser.parse(Statement::parse)?;
                if else_stmt.is_binding() {
                    return Err(parser.error(
                        ErrorKind::illegal_statement("Can not having binding statement alone"),
                        None,
                    ));
                }
                Ok(ElseBlock {
                    else_tok,
                    else_stmt: Box::new(else_stmt),
                })
            })?;
            Statement::If(StatementIf {
                if_tok,
                lparen,
                condition,
                rparen,
                then_stmt: Box::new(then_stmt),
                else_block,
            })
        }
        TokenKind::While => {
            let while_tok = parser.parse(While::parse)?;
            let lparen = parser.parse(LParen::parse)?;
            let condition = parser.parse(Expr::parse)?;
            let rparen = parser.parse(RParen::parse)?;

            let then_stmt = parser.parse(Statement::parse)?;
            Statement::While(StatementWhile {
                while_tok,
                lparen,
                condition,
                rparen,
                stmt: Box::new(then_stmt),
            })
        }
        TokenKind::Loop => {
            let loop_tok = parser.parse(Loop::parse)?;
            let block = parser.parse(StatementBlock::parse)?;
            Statement::Loop(StatementLoop {
                loop_tok,
                block: Box::new(block),
            })
        }
        TokenKind::Try => {
            let try_tok = parser.parse(Try::parse)?;
            let block = parser.parse(StatementBlock::parse)?;
            let catches = parser.parse(multi1(|parser: &mut SyntacticParser<'p, R>| {
                let catch = parser.parse(Catch::parse)?;
                let lparen = parser.parse(LParen::parse)?;
                let binding = parser.parse(OptTypeBinding::parse)?;
                let rparen = parser.parse(RParen::parse)?;
                let block = parser.parse(StatementBlock::parse)?;
                Ok(CatchBlock {
                    catch,
                    lparen,
                    binding,
                    rparen,
                    statement_block: block,
                })
            }))?;
            Statement::TryCatch(StatementTryCatch {
                try_tok,
                statement_block: block,
                catches,
            })
        }
        TokenKind::Return => {
            let ret = parser.parse(Return::parse)?;
            let (return_expr, end) = if End::could_parse(parser)? {
                (None, parser.parse(End::parse)?)
            } else {
                let expr = parser.parse(Expr::parse)?;
                (Some(expr), parser.parse(End::parse)?)
            };
            Statement::Return(StatementReturn {
                ret,
                expr: return_expr,
                end,
            })
        }
        _ => {
            let e: Expr = parser.parse(Expr::parse)?;
            let end = parser.try_parse(End::parse)?;
            Statement::Expr(StatementExpr { expr: e, end })
        }
    };
    Ok(statement)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::syntactic_parser::tests::test_parser;
    use crate::parser::Err;
    use test_log::test;

    #[test]
    fn test_parse_statement_list() {
        test_parser(
            r#"
            let i: Int = 3
            i.method()
            let b = 2; const c = 3
        "#,
            |parser, _| {
                let parsed = parser.parse(parse_statement_list).unwrap();
                assert_eq!(
                    parsed.statements.len(),
                    4,
                    "should have 4 statements but found {}",
                    parsed.statements.len()
                );
                println!("{parsed:#?}")
            },
        );
    }

    #[test]
    fn test_parse_illegal_statement_list() {
        test_parser(
            r#"
            let i: Int = 3
            i + 1
            let b = 2; const c = 3
        "#,
            |parser, _| {
                let parsed = parser.parse(parse_statement_list).unwrap_err();
                assert!(
                    matches!(parsed, Err::Error(error) if matches!(error.kind, ErrorKind::IllegalStatement { .. }))
                );
            },
        );
    }

    #[test]
    fn test_parse_if_no_else() {
        test_parser(
            r#"
            if (true) 3+3;
        "#,
            |parser, _| {
                let parsed = parser.parse(parse_statement).unwrap();
                assert!(
                    matches!(parsed, Statement::If(_)),
                    "expected if: {:#?}",
                    parsed
                );
                let Statement::If(StatementIf {
                    if_tok,
                    lparen,
                    condition,
                    rparen,
                    then_stmt,
                    else_block,
                }) = parsed
                else {
                    panic!()
                };
                assert!(matches!(else_block, None));
            },
        );
    }
    #[test]
    fn test_parse_if_else() {
        test_parser(
            r#"
            if (true) 3+3 else { }
        "#,
            |parser, _| {
                let parsed = parser.parse(parse_statement).unwrap();
                assert!(
                    matches!(parsed, Statement::If(_)),
                    "expected if: {:#?}",
                    parsed
                );
                let Statement::If(StatementIf {
                    if_tok,
                    lparen,
                    condition,
                    rparen,
                    then_stmt,
                    else_block,
                }) = parsed
                else {
                    panic!()
                };
                assert!(matches!(else_block, Some(_)));
            },
        );
    }

    #[test]
    fn test_parse_if_else_chain() {
        test_parser(
            r#"
            if (true) 3+3 else if (false) { } else {}
        "#,
            |parser, _| {
                let parsed = parser.parse(parse_statement).unwrap();
                assert!(
                    matches!(parsed, Statement::If(_)),
                    "expected if: {:#?}",
                    parsed
                );
                let Statement::If(StatementIf {
                    if_tok,
                    lparen,
                    condition,
                    rparen,
                    then_stmt,
                    else_block,
                }) = parsed
                else {
                    panic!()
                };
                let Some(ElseBlock { else_stmt, .. }) = else_block else {
                    panic!("must have first else stmt defined")
                };
                let Statement::If(StatementIf {
                    if_tok,
                    lparen,
                    condition,
                    rparen,
                    then_stmt,
                    else_block,
                }) = *else_stmt
                else {
                    panic!("nested if")
                };
                assert!(
                    matches!(else_block, Some(_)),
                    "nested if must be owner of second else block"
                );
            },
        );
    }

    #[test]
    fn test_parse_try_catch() {
        test_parser(
            r#"
            try {
                print("hello, world");
            } catch(e: Exception) {

            }
        "#,
            |parser, _| {
                let parsed = parser.parse(parse_statement).unwrap();
                let Statement::TryCatch(try_catch) = parsed else {
                    panic!("expected try-catch {:#?}", parsed);
                };
                assert_eq!(try_catch.catches.len(), 1, "one catch expected");
            },
        );
    }
}
