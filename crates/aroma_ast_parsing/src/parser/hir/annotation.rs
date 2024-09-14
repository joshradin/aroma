//! annotations are used to mark information in some way

use crate::parser::hir::constants::{Constant, ConstantKind};
use crate::parser::hir::expr::Expr;
use crate::parser::hir::singletons::{
    Assign, Comma, DocComment, Hash, LBracket, LParen, RBracket, RParen,
};
use crate::parser::hir::{
    cut, ErrorKind, Punctuated0, SyntaxError,
};
use aroma_tokens::id::Id;
use aroma_tokens::token::{ToTokens, TokenKind};
use std::io::Read;
use crate::parser::blocking::BlockingParser;
use crate::parser::SyntaxResult;
use crate::parser::hir_parser::blocking::{CouldParse, Parsable};

/// An annotation, used to mark information that would otherwise not be possible to represent
#[derive(Debug, ToTokens)]
pub struct Annotation {
    pub hash: Hash,
    pub lbracket: LBracket,
    pub meta: Meta,
    pub rbracket: RBracket,
}

#[derive(Debug, ToTokens)]
pub enum Meta {
    Id(Id),
    List(MetaList),
    NameValue(MetaNameValue),
}

#[derive(Debug, ToTokens)]
pub struct MetaList {
    pub id: Id,
    pub lparen: LParen,
    pub items: Punctuated0<Meta, Comma>,
    pub rparen: RParen,
}

#[derive(Debug, ToTokens)]
pub struct MetaNameValue {
    pub id: Id,
    pub assign: Assign,
    pub value: Expr,
}

impl Parsable for Annotation {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
        parse_annotation(parser)
    }
}

impl CouldParse for Annotation {
    fn could_parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<bool> {
        Ok(Hash::could_parse(parser)? || DocComment::could_parse(parser)?)
    }
}

impl Parsable for Meta {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
        parse_meta(parser)
    }
}

impl CouldParse for Meta {
    fn could_parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<bool> {
        Id::could_parse(parser)
    }
}

fn parse_annotation<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Annotation> {
    if DocComment::could_parse(parser)? {
        let doc_comment = parser.parse(DocComment::parse)?;
        let comment = doc_comment.comment().to_string();
        Ok(Annotation {
            hash: Default::default(),
            lbracket: Default::default(),
            meta: Meta::NameValue(MetaNameValue {
                id: Id::from("doc"),
                assign: Default::default(),
                value: Expr::Constant(Constant {
                    kind: ConstantKind::String(comment),
                    tok: doc_comment.token.clone(),
                }),
            }),
            rbracket: Default::default(),
        })
    } else {
        let hash = parser.parse(Hash::parse)?;
        let lbracket = parser.parse(LBracket::parse)?;

        let meta = parser.parse(cut(parse_meta))?;

        let rbracket = parser.parse(RBracket::parse)?;
        Ok(Annotation {
            hash,
            lbracket,
            meta,
            rbracket,
        })
    }
}

fn parse_doc_comment<R: Read>(parser: &mut BlockingParser<R>) -> SyntaxResult<Annotation> {
    todo!()
}

fn parse_meta<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Meta> {
    let id = parser.parse(Id::parse)?;
    let peek = parser
        .peek()?
        .cloned()
        .ok_or_else(|| parser.error(ErrorKind::UnexpectedEof, None))?;
    match peek.kind() {
        TokenKind::RParen => Ok(Meta::Id(id)),
        TokenKind::LParen => {
            let lparen = parser.parse(LParen::parse)?;
            let items = parser.parse(cut(Punctuated0::<Meta, Comma>::parse))?;
            let rparen = parser.parse(RParen::parse)?;
            Ok(Meta::List(MetaList {
                id,
                lparen,
                items,
                rparen,
            }))
        }
        TokenKind::Assign => {
            let assign = parser.parse(Assign::parse)?;
            let expr = parser.parse(Expr::parse)?;
            Ok(Meta::NameValue(MetaNameValue {
                id,
                assign,
                value: expr,
            }))
        }
        _ => Err(parser.error(ErrorKind::expected_token(["(", ")", "="], peek), None)),
    }
}
