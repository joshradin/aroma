//! statements

use crate::parser::binding::OptTypeBinding;
use crate::parser::expr::{remove_nl, Expr};
use crate::parser::singletons::*;
use crate::parser::syntactic_parser::hir::helpers::End;
use crate::parser::{
    cut, multi1, CouldParse, ErrorKind, Parsable, SyntacticParser, SyntaxError, SyntaxResult,
};
use aroma_tokens::token::{ToTokens, TokenKind};
use tracing::debug;
use std::io::Read;

/// General statement types
#[derive(Debug, ToTokens)]
pub enum Statement {
    Let(LetStatement),
    Const(ConstStatement),
    Assign(AssignStatement),
    Expr(ExprStatement),
    Block(BlockStatement),
    If(IfStatement),
    While(WhileStatemeny),
    Loop(LookStatement),
    Return(ReturnStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Match(MatchStatement),
    TryCatch(TryCatchStatement),
}

impl Statement {
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

impl Parsable for Statement {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
        parse_statement(parser)
    }
}
/// A list of statements
#[derive(Debug, ToTokens)]
pub struct StatementList {
    pub statements: Vec<Statement>,
}
impl Parsable for StatementList {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
        parse_statement_list(parser)
    }
}
/// A block of statements
#[derive(Debug, ToTokens)]
pub struct BlockStatement {
    pub lcurly: LCurly,
    pub list: StatementList,
    pub rcurly: RCurly,
}

impl Parsable for BlockStatement {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
        let lcurly = parser.parse(LCurly::parse)?;
        let list = parser.parse(StatementList::parse)?;
        let rcurly = parser.parse(RCurly::parse)?;
        Ok(BlockStatement {
            lcurly,
            list,
            rcurly,
        })
    }
}

#[derive(Debug, ToTokens)]
pub struct LetStatement {
    pub let_tok: Let,
    pub binding: OptTypeBinding,
    pub assign: Assign,
    pub expr: Expr,
    pub end: End,
}

#[derive(Debug, ToTokens)]
pub struct ConstStatement {
    pub const_tok: Const,
    pub binding: OptTypeBinding,
    pub assign: Assign,
    pub expr: Expr,
    pub end: End,
}

#[derive(Debug, ToTokens)]
pub struct AssignStatement {
    pub lvalue: Expr,
    pub assign: Assign,
    pub rvalue: Expr,
    pub end: End,
}

/// If statement
#[derive(Debug, ToTokens)]
pub struct IfStatement {
    pub if_tok: If,
    pub lparen: LParen,
    pub condition: Expr,
    pub rparen: RParen,
    pub then_stmt: Box<Statement>,
    pub else_block: Option<ElseBlock>,
}

/// while statement
#[derive(Debug, ToTokens)]
pub struct WhileStatemeny {
    pub while_tok: While,
    pub lparen: LParen,
    pub condition: Expr,
    pub rparen: RParen,
    pub stmt: Box<Statement>,
}

/// for statement
#[derive(Debug, ToTokens)]
pub struct StatementFor {
    pub for_tok: For,
    pub lparen: LParen,
    pub condition: Expr,
    pub rparen: RParen,
    pub stmt: Box<Statement>,
}

/// for statement
#[derive(Debug, ToTokens)]
pub struct LookStatement {
    pub loop_tok: Loop,
    pub block: Box<BlockStatement>,
}

#[derive(Debug, ToTokens)]
pub struct ElseBlock {
    pub else_tok: Else,
    pub else_stmt: Box<Statement>,
}

/// A statement consisting of a single expression that must ultimately be a function call.
#[derive(Debug, ToTokens)]
pub struct ExprStatement {
    pub expr: Expr,
    pub end: Option<End>,
}

#[derive(Debug, ToTokens)]
pub struct ReturnStatement {
    pub ret: Return,
    pub expr: Option<Expr>,
    pub end: End,
}

#[derive(Debug, ToTokens)]
pub struct BreakStatement {
    pub break_tok: Break,
    pub end: End,
}

#[derive(Debug, ToTokens)]
pub struct ContinueStatement {
    pub break_tok: Break,
    pub end: End,
}

#[derive(Debug, ToTokens)]
pub struct MatchStatement {
    pub match_tok: Match,
    pub lparen: LParen,
    pub condition: Expr,
    pub rparen: RParen,
}

#[derive(Debug, ToTokens)]
pub struct TryCatchStatement {
    pub try_tok: Try,
    pub statement_block: BlockStatement,
    pub catches: Vec<CatchBlock>,
}

#[derive(Debug, ToTokens)]
pub struct CatchBlock {
    pub catch: Catch,
    pub lparen: LParen,
    pub binding: OptTypeBinding,
    pub rparen: RParen,
    pub statement_block: BlockStatement,
}

/// Parse a statement block
fn parse_statement_list<'p, R: Read>(
    parser: &mut SyntacticParser<R>,
) -> SyntaxResult<StatementList> {
    let mut list = vec![];
    parser.with_ignore_nl(false, |parser| {
        while !parser.eof() {
            while parser.parse_opt::<End>()?.is_some() {}
            if parser.eof() || RCurly::could_parse(parser)? {
                break;
            }
            let statement = match parser.parse(Statement::parse) {
                Ok(statement) => statement,
                Err(e) => {
                    eprintln!(
                        "encountered {e:?} while parser is in state {:#?}",
                        &parser.state
                    );
                    return Err(e);
                }
            };
            list.push(statement);
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
    parser: &mut SyntacticParser<R>,
) -> crate::parser::SyntaxResult<Statement> {
    parser.parse(remove_nl)?;
    let lookahead = parser
        .peek()?
        .cloned()
        .ok_or::<SyntaxError>(ErrorKind::UnexpectedEof.into())?;
    let statement: Statement = match lookahead.kind() {
        TokenKind::Let => {
            let let_tok = parser.parse(Let::parse)?;
            let binding = parser.parse(cut(OptTypeBinding::parse))?;
            let assign = parser.parse(cut(Assign::parse))?;
            let val = parser.parse(cut(Expr::parse))?;
            let end = parser.parse(cut(End::parse))?;

            Statement::Let(LetStatement {
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

            Statement::Const(ConstStatement {
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

            let else_block = parser.try_parse(|parser: &mut SyntacticParser<R>| {
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
            Statement::If(IfStatement {
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
            Statement::While(WhileStatemeny {
                while_tok,
                lparen,
                condition,
                rparen,
                stmt: Box::new(then_stmt),
            })
        }
        TokenKind::Loop => {
            let loop_tok = parser.parse(Loop::parse)?;
            let block = parser.parse(BlockStatement::parse)?;
            Statement::Loop(LookStatement {
                loop_tok,
                block: Box::new(block),
            })
        }
        TokenKind::Try => {
            let try_tok = parser.parse(Try::parse)?;
            let block = parser.parse(BlockStatement::parse)?;
            let catches = parser.parse(multi1(|parser: &mut SyntacticParser<R>| {
                let catch = parser.parse(Catch::parse)?;
                let lparen = parser.parse(LParen::parse)?;
                let binding = parser.parse(OptTypeBinding::parse)?;
                let rparen = parser.parse(RParen::parse)?;
                let block = parser.parse(BlockStatement::parse)?;
                Ok(CatchBlock {
                    catch,
                    lparen,
                    binding,
                    rparen,
                    statement_block: block,
                })
            }))?;
            Statement::TryCatch(TryCatchStatement {
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
            Statement::Return(ReturnStatement {
                ret,
                expr: return_expr,
                end,
            })
        }
        _ => {
            debug!("parsing expression");
            let e: Expr = parser.parse(Expr::parse)?;
            if let Some(assign) = parser.parse_opt::<Assign>()? {
                let rvalue = parser.parse(Expr::parse).map_err(|e| e.cut())?;
                let end = parser.parse(End::parse)?;
                Statement::Assign(AssignStatement {
                    lvalue: e,
                    assign,
                    rvalue,
                    end,
                })
            } else {
                let end = parser.try_parse(End::parse)?;
                Statement::Expr(ExprStatement { expr: e, end })
            }
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
            let b = 2; const c = 3;
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
                    matches!(parsed, Err::Error(error) if matches!(error.kind.error(), ErrorKind::IllegalStatement { .. }))
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
                let Statement::If(IfStatement {
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
                let Statement::If(IfStatement {
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
                let Statement::If(IfStatement {
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
                let Statement::If(IfStatement {
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
