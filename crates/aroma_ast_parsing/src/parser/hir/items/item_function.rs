use crate::parser::blocking::BlockingParser;
use crate::parser::hir::annotation::Annotation;
use crate::parser::hir::binding::{FnParameters, Type};
use crate::parser::hir::items::{
    parse_generics, FnBody, FnReturn, FnThrows, GenericDeclarations, Visibility,
};
use crate::parser::hir::singletons::{Arrow, Comma, Static, Throws, VarId};
use crate::parser::hir::statement::BlockStatement;
use crate::parser::hir::{
    cut, singletons, Punctuated1
};
use crate::parser::hir_parser::blocking::{CouldParse, Parsable};
use crate::parser::SyntaxResult;
use aroma_tokens::token::ToTokens;
use std::io::Read;
use tracing::instrument;

/// A function declaration
#[derive(Debug, ToTokens)]
pub struct ItemFn {
    pub annotations: Vec<Annotation>,
    pub vis: Option<Visibility>,
    pub static_tok: Option<Static>,
    pub fn_tok: singletons::Fn,
    pub ident: VarId,
    pub generics: Option<GenericDeclarations>,
    pub fn_parameters: FnParameters,
    pub fn_return: Option<FnReturn>,
    pub fn_throws: Option<FnThrows>,
    pub body: FnBody,
}

#[instrument(skip_all)]
pub fn parse_function<R: Read>(
    annotations: Vec<Annotation>,
    visibility: Option<Visibility>,
    parser: &mut BlockingParser<'_, R>,
) -> SyntaxResult<ItemFn> {
    let fn_tok = parser.parse(singletons::Fn::parse)?;
    let name = parser.parse(VarId::parse)?;
    let generics = parser.parse(parse_generics)?;
    let parameters = parser.parse(cut(FnParameters::parse))?;
    let fn_return = if Arrow::could_parse(parser)? {
        let arrow = parser.parse(Arrow::parse)?;
        let returns = parser.parse(Type::parse)?;
        Some(FnReturn { arrow, returns })
    } else {
        None
    };

    let fn_throws = if Throws::could_parse(parser)? {
        let throws = parser.parse(Throws::parse)?;
        let types = parser.parse(Punctuated1::<Type, Comma>::parse)?;
        Some(FnThrows { throws, types })
    } else {
        None
    };

    let body = FnBody {
        body: parser.parse(BlockStatement::parse)?,
    };
    let fn_ = ItemFn {
        annotations,
        vis: visibility,
        static_tok: None,
        fn_tok,
        ident: name,
        generics,
        fn_parameters: parameters,
        fn_return,
        fn_throws,
        body,
    };
    Ok(fn_)
}
