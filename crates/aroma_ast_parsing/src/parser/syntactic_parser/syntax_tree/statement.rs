//! statements

use crate::parser::binding::{Binding, OptTypeBinding};
use crate::parser::expr::{remove_nl, Expr};
use crate::parser::singletons::*;
use crate::parser::{cut, Error, ErrorKind, Parsable, SyntacticParser};
use aroma_ast::token::{ToTokens, TokenKind};
use std::io::Read;

/// General statement types
#[derive(Debug, ToTokens)]
pub enum Statement<'p> {
    Let(StatementLet<'p>),
    Const(StatementConst<'p>),
    Block(StatementBlock<'p>),
    Expr(StatementExpr<'p>),
}
impl<'p> Parsable<'p> for Statement<'p> {
    type Err = Error<'p>;

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
    type Err = Error<'p>;

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
    type Err = Error<'p>;

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
    pub end: StatementEnd<'p>,
}

#[derive(Debug, ToTokens)]
pub struct StatementConst<'p> {
    pub const_tok: Const<'p>,
    pub binding: OptTypeBinding<'p>,
    pub assign: Assign<'p>,
    pub expr: Expr<'p>,
    pub end: StatementEnd<'p>,
}

#[derive(Debug, ToTokens)]
pub struct StatementIf<'p> {
    pub if_tok: If<'p>,
}

/// A statement consisting of a single expression that must ultimately be a function call.
#[derive(Debug, ToTokens)]
pub struct StatementExpr<'p> {
    pub expr: Expr<'p>,
}

/// End of statement
#[derive(Debug, ToTokens)]
pub enum StatementEnd<'p> {
    SemiC(SemiC<'p>),
    Nl(Nl<'p>),
}

impl<'p> Parsable<'p> for StatementEnd<'p> {
    type Err = Error<'p>;

    fn parse<R: Read>(
        parser: &mut SyntacticParser<'p, R>,
    ) -> Result<Self, crate::parser::Err<Self::Err>> {
        let p = parser
            .consume()?
            .ok_or_else(|| parser.error(ErrorKind::UnexpectedEof, None))?;
        match p.kind() {
            TokenKind::SemiColon => {
                let c = SemiC::try_from(p)?;
                Ok(StatementEnd::SemiC(c))
            }
            TokenKind::Nl => {
                let c = Nl::try_from(p)?;
                Ok(StatementEnd::Nl(c))
            }
            _ => Err(parser.error(ErrorKind::expected_token([";", "\\n"], p), None)),
        }
    }
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
    Ok(StatementList { statements: list })
}
/// Parse a statement block
fn parse_statement<'p, R: Read>(
    parser: &mut SyntacticParser<'p, R>,
) -> crate::parser::Result<'p, Statement<'p>> {
    parser.parse(remove_nl)?;
    let lookahead = parser.peek()?.cloned().ok_or::<Error<'p>>(ErrorKind::UnexpectedEof.into())?;
    let statement: Statement<'p> = match lookahead.kind() {
        TokenKind::Let => {
            let let_tok = parser.parse(Let::parse)?;
            let binding = parser.parse(cut(OptTypeBinding::parse))?;
            let assign = parser.parse(cut(Assign::parse))?;
            let val = parser.parse(cut(Expr::parse))?;
            let end = parser.parse(cut(StatementEnd::parse))?;

            Statement::Let(
                StatementLet {
                    let_tok,
                    binding,
                    assign,
                    expr: val,
                    end,
                }
            )
        }
        TokenKind::Const => {
            let const_tok = parser.parse(Const::parse)?;
            let binding = parser.parse(cut(OptTypeBinding::parse))?;
            let assign = parser.parse(cut(Assign::parse))?;
            let val = parser.parse(cut(Expr::parse))?;
            let end = parser.parse(cut(StatementEnd::parse))?;

            Statement::Const(
                StatementConst {
                    const_tok,
                    binding,
                    assign,
                    expr: val,
                    end,
                }
            )
        }
        TokenKind::If => {
            todo!("if statement")
        },
        _ => {
            let e: Expr = parser.parse(Expr::parse)?;
            let end = parser.try_parse(StatementEnd::parse)?;
            Statement::Expr(StatementExpr {
                expr: e,
            })
        }
    };
    Ok(statement)
}


#[cfg(test)]
mod tests {
    use crate::parser::statement::parse_statement_list;
    use crate::parser::syntactic_parser::tests::test_parser;

    #[test]
    fn test_parse_statement_list() {
        test_parser(r#"
            let i: Int = 3
            let b = 2; const c = 3
        "#, |parser, _| {
            let parsed = parser.parse(parse_statement_list).unwrap();
            assert_eq!(parsed.statements.len(), 3, "should have 2 statements but found {}", parsed.statements.len());
            println!("{parsed:#?}")
        });
    }
}