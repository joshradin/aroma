use crate::parser::blocking::{BlockingParser, CouldParse, Parsable};
use crate::parser::hir::annotation::Annotation;
use crate::parser::hir::binding::{FnParameters, Type};
use crate::parser::hir::constants::Constant;
use crate::parser::hir::items::{
    parse_generics, FnReturn, FnThrows, GenericDeclarations, Visibility,
};
use crate::parser::hir::singletons::{Arrow, Comma, LParen, Native, RParen, Static, Throws, VarId};
use crate::parser::hir::{cut, singletons, End, Punctuated1};
use crate::parser::SyntaxResult;
use aroma_tokens::token::ToTokens;
use std::io::Read;
use tracing::trace;

/// A native function declaration
#[derive(Debug, ToTokens)]
pub struct ItemNativeFn {
    pub vis: Option<Visibility>,
    pub native_tok: Native,
    pub native_link: Option<NativeLink>,
    pub fn_tok: singletons::Fn,
    pub ident: VarId,
    pub generics: Option<GenericDeclarations>,
    pub fn_parameters: FnParameters,
    pub fn_return: Option<FnReturn>,
    pub fn_throws: Option<FnThrows>,
    pub end: End,
}

/// The name to link to, if not present the ident is used
#[derive(Debug, ToTokens)]
pub struct NativeLink {
    pub lparen: LParen,
    pub name: Constant,
    pub rparen: RParen,
}

pub fn parse_native_function<R: Read>(
    annotations: Vec<Annotation>,
    visibility: Option<Visibility>,
    mut parser: &mut BlockingParser<R>,
) -> SyntaxResult<ItemNativeFn> {
    let native_tok = parser.parse(Native::parse)?;
    let native_link = if LParen::could_parse(parser)? {
        let lparen = parser.parse(LParen::parse)?;
        let name = parser.parse(Constant::parse)?;
        let rparen = parser.parse(RParen::parse)?;
        Some(NativeLink {
            lparen,
            name,
            rparen,
        })
    } else {
        None
    };

    let fn_tok = parser.parse(singletons::Fn::parse)?;
    let name = parser.parse(VarId::parse)?;
    let generics = parser.parse(parse_generics)?;
    let parameters = parser.parse(cut(FnParameters::parse))?;
    trace!("trying to parse return");
    let fn_return = if Arrow::could_parse(parser)? {
        let arrow = parser.parse(Arrow::parse)?;
        let returns = parser.parse(Type::parse)?;
        Some(FnReturn { arrow, returns })
    } else {
        None
    };
    trace!("returns: {fn_return:?}");
    trace!("trying to parse throws");
    let fn_throws = if Throws::could_parse(parser)? {
        trace!("found throws token");
        let throws = parser.parse(Throws::parse)?;
        let types = parser.parse(Punctuated1::<Type, Comma>::parse)?;
        Some(FnThrows { throws, types })
    } else {
        None
    };
    trace!("throws: {fn_throws:?}");
    let end = parser.parse(End::parse)?;
    Ok(ItemNativeFn {
        vis: visibility,
        native_tok,
        native_link,
        fn_tok,
        ident: name,
        generics,
        fn_parameters: parameters,
        fn_return,
        fn_throws,
        end,
    })
}
