use crate::parser::hir::annotation::Annotation;
use crate::parser::hir::binding::{Binding, FnParameters, Type};
use crate::parser::hir::items::item_class::{
    ClassExtends, ClassMembers, FnReturn, FnThrows, ItemClass,
};
use crate::parser::hir::items::{
    parse_generics, ClassFieldDefaultValue, ClassMember, FnBody, GenericDeclarations,
    ItemAbstractFn, ItemFn, Visibility,
};
use crate::parser::hir::singletons::{
    Abstract, Arrow, Comma, Extends, Final, Interface, LCurly, Public, RCurly, Static, Throws,
    VarId,
};
use crate::parser::hir::statement::BlockStatement;
use crate::parser::hir::{cut, singletons, End, ErrorKind, Punctuated1};
use crate::parser::traits::{CouldParse, Parsable};
use aroma_tokens::token::{ToTokens, TokenKind};
use std::io::Read;
use tracing::{debug, instrument, trace};
use crate::parser::blocking::SyntacticParser;
use crate::parser::SyntaxResult;

/// An interface declaration
#[derive(Debug, ToTokens)]
pub struct ItemInterface {
    pub vis: Option<Visibility>,
    pub interface: Interface,
    pub ident: VarId,
    pub generics: Option<GenericDeclarations>,
    pub extends: Option<InterfaceExtends>,
    pub members: InterfaceMembers,
}

/// Class extends clause
#[derive(Debug, ToTokens)]
pub struct InterfaceExtends {
    pub extends: Extends,
    pub types: Punctuated1<Type, Comma>,
}

/// An abstract function declared in an interface declaration
#[derive(Debug, ToTokens)]
pub struct ItemInterfaceFn {
    pub vis: Option<Public>,
    pub fn_tok: singletons::Fn,
    pub ident: VarId,
    pub generics: Option<GenericDeclarations>,
    pub fn_parameters: FnParameters,
    pub fn_return: Option<FnReturn>,
    pub fn_throws: Option<FnThrows>,
    pub end: End,
}

/// Class field
#[derive(Debug, ToTokens)]
pub struct InterfaceStaticField {
    pub vis: Option<Public>,
    pub static_tok: Static,
    pub final_tok: Option<Final>,
    pub binding: Binding,
    pub value: ClassFieldDefaultValue,
    pub end: End,
}

/// Class member
#[derive(Debug, ToTokens)]
pub enum InterfaceMember {
    AbstractMethod(ItemInterfaceFn),
    StaticMethod(ItemFn),
    Class(ItemClass),
    StaticField(InterfaceStaticField),
}

/// All class members
#[derive(Debug, ToTokens)]
pub struct InterfaceMembers {
    pub lcurly: LCurly,
    pub members: Vec<InterfaceMember>,
    pub rcurly: RCurly,
}

#[instrument(skip_all)]
pub fn parse_interface<R: Read>(
    visibility: Option<Visibility>,
    parser: &mut SyntacticParser<'_, R>,
) -> SyntaxResult<ItemInterface> {
    let interface = parser.parse(Interface::parse)?;
    let id = parser.parse(VarId::parse)?;
    let generics = parse_generics(parser)?;

    let extends = if Extends::could_parse(parser)? {
        Some(parser.parse(|p: &mut SyntacticParser<'_, R>| {
            let extends = p.parse(Extends::parse)?;
            let extended = p.parse(Punctuated1::parse)?;
            Ok(InterfaceExtends {
                extends,
                types: extended,
            })
        })?)
    } else {
        None
    };

    let members = parse_interface_members(&id, parser)?;
    Ok(ItemInterface {
        vis: visibility,
        interface,
        ident: id,
        generics,
        extends,
        members,
    })
}

fn parse_interface_members<R: Read>(
    owner: &VarId,
    parser: &mut SyntacticParser<'_, R>,
) -> SyntaxResult<InterfaceMembers> {
    let lcurly = parser.parse(LCurly::parse)?;
    let mut members = vec![];
    while !RCurly::could_parse(parser)? {
        parser.with_ignore_nl(false, |parser| {
            let annotations = parser.parse(Vec::<Annotation>::parse)?;
            let visibility = parser.parse_opt::<Public>()?;
            let is_static = parser.parse_opt::<Static>()?;
            let member = parse_interface_member(annotations, owner, visibility, is_static, parser)?;
            members.push(member);
            Ok(())
        })?;
    }
    let rcurly = parser.parse(RCurly::parse)?;
    Ok(InterfaceMembers {
        lcurly,
        members,
        rcurly,
    })
}

fn parse_interface_member<R: Read>(
    annotations: Vec<Annotation>,
    owner_id: &VarId,
    pub_vis: Option<Public>,
    static_tok: Option<Static>,
    parser: &mut SyntacticParser<R>,
) -> SyntaxResult<InterfaceMember> {
    let lookahead = parser.peek()?.cloned().ok_or_else(|| {
        parser.error(
            ErrorKind::expected_token(["fn", "class", "id", "final"], None),
            None,
        )
    })?;
    match lookahead.kind() {
        TokenKind::Identifier(_) | TokenKind::Final => {
            if let Some(static_tok) = static_tok {
                todo!("static field")
            } else {
                Err(parser.error(ErrorKind::InterfacesCanNotHaveObjectFields, None))
            }
        }
        TokenKind::Fn => {
            let method =
                parse_interface_method(annotations, owner_id, pub_vis, static_tok, parser)?;
            Ok(method)
        }
        _ => Err(parser.error(
            ErrorKind::expected_token(["fn", "class", "id"], lookahead),
            None,
        )),
    }
}

fn parse_interface_method<R: Read>(
    annotations: Vec<Annotation>,
    owner: &VarId,
    visibility: Option<Public>,
    is_static: Option<Static>,
    parser: &mut SyntacticParser<'_, R>,
) -> SyntaxResult<InterfaceMember> {
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

    if let Some(static_tok) = is_static {
        let body = FnBody {
            body: parser.parse(BlockStatement::parse)?,
        };
        let fn_ = ItemFn {
            annotations,
            vis: visibility.map(Visibility::Public),
            static_tok: Some(static_tok),
            fn_tok,
            ident: name,
            generics,
            fn_parameters: parameters,
            fn_return,
            fn_throws,
            body,
        };
        Ok(InterfaceMember::StaticMethod(fn_))
    } else {
        let end = parser.parse(End::parse)?;
        let abstract_fn = ItemInterfaceFn {
            vis: visibility,
            fn_tok,
            ident: name,
            generics,
            fn_parameters: parameters,
            fn_return,
            fn_throws,
            end,
        };
        Ok(InterfaceMember::AbstractMethod(abstract_fn))
    }
}
