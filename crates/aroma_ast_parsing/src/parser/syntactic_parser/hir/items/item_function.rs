use std::io::Read;
use aroma_tokens::token::ToTokens;
use crate::parser::items::{parse_generics, FnBody, FnReturn, FnThrows, GenericDeclarations, Visibility};
use crate::parser::{cut, singletons, CouldParse, Parsable, Punctuated1, SyntacticParser, SyntaxResult};
use crate::parser::binding::{FnParameters, Type};
use crate::parser::singletons::{Arrow, Comma, Static, Throws, VarId};
use crate::parser::statement::BlockStatement;

/// A function declaration
#[derive(Debug, ToTokens)]
pub struct ItemFn {
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

pub fn parse_function<R: Read>(
    visibility: Option<Visibility>,
    parser: &mut SyntacticParser<'_, R>,
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