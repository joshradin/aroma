//! items, like functions, and classes

use crate::parser::binding::{Binding, FnParameters, Type};
use crate::parser::expr::{remove_nl, Expr};
use crate::parser::singletons::{Abstract, Class, Private, Protected, Public};
use crate::parser::statement::BlockStatement;
use crate::parser::syntactic_parser::hir::helpers::End;
use crate::parser::{
    cut, map, multi0, seperated_list1, singletons::*, CouldParse, ErrorKind, Parsable, Punctuated1,
    SyntacticParser, SyntaxError, SyntaxResult,
};
use aroma_tokens::id::Id;
use aroma_tokens::spanned::Spanned;
use aroma_tokens::token::{ToTokens, TokenKind};
use aroma_types::class::{AsClassRef, ClassInst};
use aroma_types::hierarchy::intrinsics::OBJECT_CLASS;
use aroma_types::vis::Vis;
use log::{debug, trace};
use std::io::Read;
use std::result;

#[derive(Debug, ToTokens)]
pub enum Visibility {
    Public(Public),
    Protected(Protected),
    Private(Private),
}

impl Parsable for Visibility {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
        let next = parser
            .peek()?
            .cloned()
            .ok_or_else(|| parser.error(ErrorKind::UnexpectedEof, None))?;
        let vis = match next.kind() {
            TokenKind::Public => Visibility::Public(parser.parse(Public::parse)?),

            TokenKind::Private => Visibility::Private(parser.parse(Private::parse)?),
            TokenKind::Protected => Visibility::Protected(parser.parse(Protected::parse)?),
            _ => {
                return Err(parser.error(
                    ErrorKind::expected_token(["public", "private", "protected"], next),
                    None,
                ));
            }
        };
        Ok(vis)
    }
}

impl CouldParse for Visibility {
    fn could_parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<bool> {
        Ok(parser
            .peek()?
            .map(|opt| {
                matches!(
                    opt.kind(),
                    TokenKind::Public | TokenKind::Private | TokenKind::Protected
                )
            })
            .unwrap_or(false))
    }
}

impl From<Visibility> for Vis {
    fn from(value: Visibility) -> Self {
        Vis::from(&value)
    }
}

impl From<&Visibility> for Vis {
    fn from(value: &Visibility) -> Self {
        match value {
            Visibility::Public(_) => Vis::Public,
            Visibility::Protected(_) => Vis::Protected,
            Visibility::Private(_) => Vis::Private,
        }
    }
}

/// Generic declarations
#[derive(Debug, ToTokens)]
pub struct GenericDeclarations {
    pub lbracket: LBracket,
    pub bounds: Punctuated1<GenericDeclaration, Comma>,
    pub rbracket: RBracket,
}
/// A generic declaration
#[derive(Debug, ToTokens)]
pub struct GenericDeclaration {
    pub id: VarId,
    pub bound: Option<Type>,
}

impl Into<aroma_types::generic::GenericDeclaration> for &GenericDeclaration {
    fn into(self) -> aroma_types::generic::GenericDeclaration {
        aroma_types::generic::GenericDeclaration::new(
            &self.id,
            self.bound
                .as_ref()
                .map(|b| b.as_class_inst())
                .unwrap_or(OBJECT_CLASS.as_class_ref().into()),
        )
    }
}

/// A class declaration
#[derive(Debug, ToTokens)]
pub struct ItemClass {
    pub vis: Option<Visibility>,
    pub abstract_tok: Option<Abstract>,
    pub class: Class,
    pub ident: VarId,
    pub generics: Option<GenericDeclarations>,
    pub extends: Option<ClassExtends>,
    pub implements: Option<ClassImplements>,
    pub members: ClassMembers,
}

/// Class extends clause
#[derive(Debug, ToTokens)]
pub struct ClassExtends {
    pub extends: Extends,
    pub extended: Type,
}

/// Class extends clause
#[derive(Debug, ToTokens)]
pub struct ClassImplements {
    pub implements: Implements,
    pub types: Punctuated1<Type, Comma>,
}

/// Class field
#[derive(Debug, ToTokens)]
pub struct ClassField {
    pub vis: Option<Visibility>,
    pub static_tok: Option<Static>,
    pub final_tok: Option<Final>,
    pub binding: Binding,
    pub default_value: Option<ClassFieldDefaultValue>,
    pub end: End,
}

/// Class constructor
#[derive(Debug, ToTokens)]
pub struct ClassConstructor {
    pub vis: Option<Visibility>,
    pub constructor: Constructor,
    pub generics: Option<GenericDeclarations>,
    pub parameters: FnParameters,
    pub fn_throws: Option<FnThrows>,
    pub body: FnBody,
}

/// Class field default value
#[derive(Debug, ToTokens)]
pub struct ClassFieldDefaultValue {
    pub assign: Assign,
    pub value: Expr,
}

/// A function declaration
#[derive(Debug, ToTokens)]
pub struct ItemFn {
    pub vis: Option<Visibility>,
    pub static_tok: Option<Static>,
    pub fn_tok: Fn,
    pub ident: VarId,
    pub generics: Option<GenericDeclarations>,
    pub fn_parameters: FnParameters,
    pub fn_return: Option<FnReturn>,
    pub fn_throws: Option<FnThrows>,
    pub body: FnBody,
}

/// Function body
#[derive(Debug, ToTokens)]
pub struct FnBody {
    pub body: BlockStatement,
}

/// An abstract function declaration
#[derive(Debug, ToTokens)]
pub struct ItemAbstractFn {
    pub vis: Option<Visibility>,
    pub abstract_tok: Abstract,
    pub fn_tok: Fn,
    pub id: VarId,
    pub generics: Option<GenericDeclarations>,
    pub parameters: FnParameters,
    pub fn_return: Option<FnReturn>,
    pub fn_throws: Option<FnThrows>,
    pub end: End,
}

/// Return statement for a function
#[derive(Debug, ToTokens)]
pub struct FnReturn {
    pub arrow: Arrow,
    pub returns: Type,
}

/// Throw clauses
#[derive(Debug, ToTokens)]
pub struct FnThrows {
    pub throws: Throws,
    pub types: Punctuated1<Type, Comma>,
}

/// Class member
#[derive(Debug, ToTokens)]
pub enum ClassMember {
    Method(ItemFn),
    AbstractMethod(ItemAbstractFn),
    Field(ClassField),
    Constructor(ClassConstructor),
    Class(ItemClass),
}

/// All class members
#[derive(Debug, ToTokens)]
pub struct ClassMembers {
    pub lcurly: LCurly,
    pub members: Vec<ClassMember>,
    pub rcurly: RCurly,
}

/// Delegate item
#[derive(Debug, ToTokens)]
pub struct ItemDelegate {
    pub vis: Option<Visibility>,
    pub delegate: Delegate,
    pub generics: Option<GenericDeclarations>,
    pub receiver: Option<DelegateReceiver>,
    pub id: VarId,
    pub parameters: FnParameters,
    pub fn_return: Option<FnReturn>,
    pub fn_throws: Option<FnThrows>,
    pub end: End,
}

#[derive(Debug, ToTokens)]
pub struct DelegateReceiver {
    pub receiver: Type,
    pub dot: Dot
}


/// An item (interface, class, function) declaration, along with its visibility
#[derive(Debug, ToTokens)]
pub enum Item {
    Class(ItemClass),
    Func(ItemFn),
}

impl Parsable for Item {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
        parse_item(parser)
    }
}

fn parse_item<'p, R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Item> {
    parser.parse(remove_nl)?;
    let vis = parser.parse_opt::<Visibility>()?;
    parser.parse(remove_nl)?;
    let lookahead = match parser.peek()?.cloned() {
        None => {
            let err = parser.error(ErrorKind::UnexpectedEof, None);
            return if vis.is_some() {
                Err(err.cut())
            } else {
                Err(err)
            };
        }
        Some(tok) => tok,
    };

    let item = match lookahead.kind() {
        TokenKind::Abstract | TokenKind::Class => {
            let class = parse_class(vis, None, parser).map_err(|e| e.cut())?;
            Item::Class(class)
        }
        TokenKind::Interface => {
            todo!("parsing interface")
        }
        TokenKind::Fn => {
            let func = parse_function(vis, parser).map_err(|e| e.cut())?;
            Item::Func(func)
        }
        TokenKind::Delegate => {
            // delegate parsing
            todo!("delegate parsing")
        }
        _other => {
            let span = lookahead.span();
            return Err(parser
                .error_with_span(
                    ErrorKind::expected_token(["abstract", "class", "interface", "fn"], lookahead),
                    None,
                    span,
                )
                .cut());
        }
    };
    Ok(item)
}

fn parse_class<R: Read>(
    visibility: Option<Visibility>,
    static_tok: Option<Static>,
    parser: &mut SyntacticParser<'_, R>,
) -> SyntaxResult<ItemClass> {
    let abstract_tok = parser.parse_opt::<Abstract>()?;
    let class = parser.parse(Class::parse)?;
    let id = parser.parse(VarId::parse)?;
    let generics = parse_generics(parser)?;

    let extends = if Extends::could_parse(parser)? {
        Some(parser.parse(|p: &mut SyntacticParser<'_, R>| {
            let extends = p.parse(Extends::parse)?;
            let ty = p.parse(Type::parse)?;
            Ok(ClassExtends {
                extends,
                extended: ty,
            })
        })?)
    } else {
        None
    };
    let implements = if Implements::could_parse(parser)? {
        Some(parser.parse(|p: &mut SyntacticParser<'_, R>| {
            let implements = p.parse(Implements::parse)?;
            let ty = p.parse(Punctuated1::<Type, Comma>::parse)?;
            Ok(ClassImplements {
                implements,
                types: ty,
            })
        })?)
    } else {
        None
    };

    let members = parse_class_members(&id, parser)?;
    Ok(ItemClass {
        vis: visibility,
        abstract_tok,
        class,
        ident: id,
        generics,
        extends,
        implements,
        members,
    })
}

fn parse_generics<'p, R: Read>(
    parser: &mut SyntacticParser<'_, R>,
) -> SyntaxResult<Option<GenericDeclarations>> {
    if LBracket::could_parse(parser)? {
        let bounds = parser.parse(|p: &mut SyntacticParser<'_, R>| {
            let lbracket = p.parse(LBracket::parse)?;
            let bounds = p
                .parse(cut(seperated_list1(
                    Comma::parse,
                    |p: &mut SyntacticParser<'_, R>| {
                        let id = p.parse(VarId::parse)?;
                        let bound = p.parse_opt::<Type>()?;
                        Ok(GenericDeclaration { id, bound })
                    },
                )))?
                .try_into()
                .map_err(|e| p.error(e, None))?;
            let rbracket = p.parse(RBracket::parse)?;
            Ok(GenericDeclarations {
                lbracket,
                bounds,
                rbracket,
            })
        })?;
        Ok(Some(bounds))
    } else {
        Ok(None)
    }
}

fn parse_class_members<'p, R: Read>(
    owner: &VarId,
    parser: &mut SyntacticParser<'_, R>,
) -> SyntaxResult<ClassMembers> {
    let lcurly = parser.parse(LCurly::parse)?;
    let mut members = vec![];
    while !RCurly::could_parse(parser)? {
        parser.with_ignore_nl(false, |parser| {
            let visibility = parser.parse_opt::<Visibility>()?;
            let is_static = parser.parse_opt::<Static>()?;
            let member = parse_class_member(owner, visibility, is_static, parser)?;
            members.push(member);
            Ok(())
        })?;
    }
    let rcurly = parser.parse(RCurly::parse)?;
    Ok(ClassMembers {
        lcurly,
        members,
        rcurly,
    })
}

fn parse_class_member<R: Read>(
    owner: &VarId,
    visibility: Option<Visibility>,
    is_static: Option<Static>,
    parser: &mut SyntacticParser<'_, R>,
) -> SyntaxResult<ClassMember> {
    let lookahead = parser.peek()?.cloned().ok_or_else(|| {
        parser.error(
            ErrorKind::expected_token(["fn", "class", "id", "final"], None),
            None,
        )
    })?;
    match lookahead.kind() {
        TokenKind::Identifier(_) | TokenKind::Final => {
            parse_field(owner, visibility, is_static, parser)
        }
        TokenKind::Constructor if is_static.is_none() => {
            parse_constructor(owner, visibility, parser)
        }
        TokenKind::Constructor if is_static.is_some() => {
            Err(parser.error(ErrorKind::ConstructorsCanNotBeStatic, None))
        }
        TokenKind::Fn | TokenKind::Abstract => parse_method(owner, visibility, is_static, parser),
        _ => Err(parser.error(
            ErrorKind::expected_token(["fn", "class", "id", "constructor"], lookahead),
            None,
        )),
    }
}

fn parse_field<R: Read>(
    owner: &VarId,
    visibility: Option<Visibility>,
    is_static: Option<Static>,
    parser: &mut SyntacticParser<'_, R>,
) -> SyntaxResult<ClassMember> {
    let final_tok = parser.parse_opt::<Final>()?;
    let binding = parser.parse(cut(Binding::parse))?;
    let default_value = if let Some(assign) = parser.parse_opt::<Assign>()? {
        let value = parser.parse(cut(Expr::parse))?;
        Some(ClassFieldDefaultValue { assign, value })
    } else {
        None
    };
    let end = parser.parse(cut(End::parse))?;

    let class_field = ClassField {
        vis: visibility,
        static_tok: is_static,
        final_tok,
        binding,
        default_value,
        end,
    };

    Ok(ClassMember::Field(class_field))
}

fn parse_method<R: Read>(
    owner: &VarId,
    visibility: Option<Visibility>,
    is_static: Option<Static>,
    parser: &mut SyntacticParser<'_, R>,
) -> SyntaxResult<ClassMember> {
    let abstract_tok = parser.parse_opt::<Abstract>()?;
    if abstract_tok.is_some() && is_static.is_some() {
        return Err(parser.error("Can not abstract static methods", None));
    }
    let fn_tok = parser.parse(Fn::parse)?;
    let name = parser.parse(VarId::parse)?;
    let generics = parser.parse(parse_generics)?;
    let parameters = parser.parse(cut(FnParameters::parse))?;
    debug!("trying to parse return");
    let fn_return = if Arrow::could_parse(parser)? {
        let arrow = parser.parse(Arrow::parse)?;
        let returns = parser.parse(Type::parse)?;
        Some(FnReturn { arrow, returns })
    } else {
        None
    };
    debug!("returns: {fn_return:?}");
    debug!("trying to parse throws");
    let fn_throws = if Throws::could_parse(parser)? {
        trace!("found throws token");
        let throws = parser.parse(Throws::parse)?;
        let types = parser.parse(Punctuated1::<Type, Comma>::parse)?;
        Some(FnThrows { throws, types })
    } else {
        None
    };
    debug!("throws: {fn_throws:?}");

    if let Some(abstract_tok) = abstract_tok {
        let end = parser.parse(End::parse)?;
        let abstract_fn = ItemAbstractFn {
            vis: visibility,
            abstract_tok,
            fn_tok,
            id: name,
            generics,
            parameters,
            fn_return,
            fn_throws,
            end,
        };
        Ok(ClassMember::AbstractMethod(abstract_fn))
    } else {
        let body = FnBody {
            body: parser.parse(BlockStatement::parse)?,
        };
        let fn_ = ItemFn {
            vis: visibility,
            static_tok: is_static,
            fn_tok,
            ident: name,
            generics,
            fn_parameters: parameters,
            fn_return,
            fn_throws,
            body,
        };
        Ok(ClassMember::Method(fn_))
    }
}

fn parse_constructor<'p, R: Read>(
    owner: &VarId,
    visibility: Option<Visibility>,
    parser: &mut SyntacticParser<'_, R>,
) -> SyntaxResult<ClassMember> {
    let constructor = parser.parse(Constructor::parse)?;
    let generics = parser.parse(parse_generics)?;
    let parameters = parser.parse(cut(FnParameters::parse))?;
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
    let class_constructor = ClassConstructor {
        vis: visibility,
        constructor,
        generics,
        parameters,
        fn_throws,
        body,
    };
    Ok(ClassMember::Constructor(class_constructor))
}

fn parse_function<R: Read>(
    visibility: Option<Visibility>,
    parser: &mut SyntacticParser<'_, R>,
) -> SyntaxResult<ItemFn> {
    let fn_tok = parser.parse(Fn::parse)?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::syntactic_parser::tests::test_parser;
    use test_log::test;

    #[test]
    fn test_parse_class() {
        test_parser(
            r#"
        public class Set[T Eq] extends Object implements Iteratable[T] {
            public abstract fn delete() -> Bool
            public abstract fn delete2() -> Bool;
            public fn get() -> T {
            }
            public fn insert() -> Bool {
            }
        }
        "#,
            |parser, _| {
                let class = parser.parse(Item::parse).unwrap_or_else(|e| panic!("{e}"));
                let Item::Class(class) = class else {
                    panic!("expected a class");
                };
                assert_eq!(class.members.members.len(), 4);
            },
        )
    }

    #[test]
    fn test_parse_function() {
        test_parser(
            r#"
            fn read[R Read](reader: R, buffer: Array[Byte]) -> Int throws IoException {

            }
        "#,
            |parser, _| {
                let item = parser.parse(Item::parse).unwrap_or_else(|e| panic!("{e}"));
                let Item::Func(func) = item else {
                    panic!("expected a function");
                };
                println!("{func:#?}");
            },
        )
    }
}
