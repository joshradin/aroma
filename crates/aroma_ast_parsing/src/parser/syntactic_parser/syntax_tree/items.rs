//! items, like functions, and classes

use crate::parser::binding::{Binding, FnParameters, Type};
use crate::parser::expr::{remove_nl, Expr};
use crate::parser::singletons::{Abstract, Class, Private, Protected, Public};
use crate::parser::statement::StatementBlock;
use crate::parser::syntactic_parser::syntax_tree::helpers::End;
use crate::parser::{
    cut, map, multi0, seperated_list1, singletons::*, CouldParse, ErrorKind, Parsable, Punctuated1,
    Result, SyntacticParser, SyntaxError,
};
use aroma_ast::id::Id;
use aroma_ast::spanned::Spanned;
use aroma_ast::token::{ToTokens, TokenKind};
use log::{debug, trace};
use std::io::Read;

#[derive(Debug, ToTokens)]
pub enum Visibility<'p> {
    Public(Public<'p>),
    Protected(Protected<'p>),
    Private(Private<'p>),
}

impl<'p> Parsable<'p> for Visibility<'p> {
    type Err = SyntaxError<'p>;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<'p, Self> {
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

impl<'p> CouldParse<'p> for Visibility<'p> {
    fn could_parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<'p, bool> {
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

/// Generic declarations
#[derive(Debug, ToTokens)]
pub struct GenericDeclarations<'p> {
    pub lbracket: LBracket<'p>,
    pub bounds: Punctuated1<GenericDeclaration<'p>, Comma<'p>>,
    pub rbracket: RBracket<'p>,
}
/// A generic declaration
#[derive(Debug, ToTokens)]
pub struct GenericDeclaration<'p> {
    pub id: VarId<'p>,
    pub bound: Option<Type<'p>>,
}

/// A class declaration
#[derive(Debug, ToTokens)]
pub struct ItemClass<'p> {
    pub vis: Option<Visibility<'p>>,
    pub abstract_tok: Option<Abstract<'p>>,
    pub class: Class<'p>,
    pub id: VarId<'p>,
    pub generics: Option<GenericDeclarations<'p>>,
    pub extends: Option<ClassExtends<'p>>,
    pub implements: Option<ClassImplements<'p>>,
    pub members: ClassMembers<'p>,
}

/// Class extends clause
#[derive(Debug, ToTokens)]
pub struct ClassExtends<'p> {
    pub extends: Extends<'p>,
    pub extended: Type<'p>,
}

/// Class extends clause
#[derive(Debug, ToTokens)]
pub struct ClassImplements<'p> {
    pub implements: Implements<'p>,
    pub types: Punctuated1<Type<'p>, Comma<'p>>,
}

/// Class field
#[derive(Debug, ToTokens)]
pub struct ClassField<'p> {
    pub vis: Option<Visibility<'p>>,
    pub static_tok: Option<Static<'p>>,
    pub final_tok: Option<Final<'p>>,
    pub binding: Binding<'p>,
    pub default_value: Option<ClassFieldDefaultValue<'p>>,
}

/// Class constructor
#[derive(Debug, ToTokens)]
pub struct ClassConstructor<'p> {
    pub vis: Option<Visibility<'p>>,
    pub constructor: Constructor<'p>,
    pub generics: Option<GenericDeclarations<'p>>,
    pub parameters: FnParameters<'p>,
    pub fn_throws: Option<FnThrows<'p>>,
    pub body: FnBody<'p>,
}

/// Class field default value
#[derive(Debug, ToTokens)]
pub struct ClassFieldDefaultValue<'p> {
    pub assign: Assign<'p>,
    pub value: Expr<'p>,
}

/// A function declaration
#[derive(Debug, ToTokens)]
pub struct ItemFn<'p> {
    pub vis: Option<Visibility<'p>>,
    pub static_tok: Option<Static<'p>>,
    pub fn_tok: Fn<'p>,
    pub id: VarId<'p>,
    pub generics: Option<GenericDeclarations<'p>>,
    pub parameters: FnParameters<'p>,
    pub fn_return: Option<FnReturn<'p>>,
    pub fn_throws: Option<FnThrows<'p>>,
    pub body: FnBody<'p>,
}

/// Function body
#[derive(Debug, ToTokens)]
pub struct FnBody<'p> {
    pub body: StatementBlock<'p>,
}

/// An abstract function declaration
#[derive(Debug, ToTokens)]
pub struct ItemAbstractFn<'p> {
    pub vis: Option<Visibility<'p>>,
    pub abstract_tok: Abstract<'p>,
    pub fn_tok: Fn<'p>,
    pub id: VarId<'p>,
    pub generics: Option<GenericDeclarations<'p>>,
    pub parameters: FnParameters<'p>,
    pub fn_return: Option<FnReturn<'p>>,
    pub fn_throws: Option<FnThrows<'p>>,
    pub end: End<'p>,
}

/// Return statement for a function
#[derive(Debug, ToTokens)]
pub struct FnReturn<'p> {
    pub arrow: Arrow<'p>,
    pub returns: Type<'p>,
}

/// Throw clauses
#[derive(Debug, ToTokens)]
pub struct FnThrows<'p> {
    pub throws: Throws<'p>,
    pub types: Punctuated1<Type<'p>, Comma<'p>>,
}

/// Class member
#[derive(Debug, ToTokens)]
pub enum ClassMember<'p> {
    Method(ItemFn<'p>),
    AbstractMethod(ItemAbstractFn<'p>),
    Field(ClassField<'p>),
    Constructor(ClassConstructor<'p>),
    Class(ItemClass<'p>),
}

/// All class members
#[derive(Debug, ToTokens)]
pub struct ClassMembers<'p> {
    pub lcurly: LCurly<'p>,
    pub members: Vec<ClassMember<'p>>,
    pub rcurly: RCurly<'p>,
}

/// An item (interface, class, function) declaration, along with its visibility
#[derive(Debug, ToTokens)]
pub enum Item<'p> {
    Class(ItemClass<'p>),
    Func(ItemFn<'p>),
}

impl<'p> Parsable<'p> for Item<'p> {
    type Err = SyntaxError<'p>;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<'p, Self> {
        parse_item(parser)
    }
}

/// Declares the current namespace
#[derive(Debug, ToTokens)]
pub struct NamespaceDeclaration<'p> {
    pub namespace: Namespace<'p>,
    pub id: Id<'p>,
    pub end: End<'p>,
}

/// Highest level of parsing, the translation unit consists of items
#[derive(Debug, ToTokens)]
pub struct TranslationUnit<'p> {
    pub namespace_declaration: Option<NamespaceDeclaration<'p>>,
    pub items: Vec<Item<'p>>,
}

impl<'p> Parsable<'p> for TranslationUnit<'p> {
    type Err = SyntaxError<'p>;

    fn parse<R: Read>(
        parser: &mut SyntacticParser<'p, R>,
    ) -> std::result::Result<Self, crate::parser::Err<Self::Err>> {
        parser.parse(remove_nl)?;
        let namespace_declaration = parser.with_ignore_nl(false, |parser| {
            if let Some(namespace) = parser.parse_opt::<Namespace>()? {
                let id = parser.parse(cut(Id::parse))?;
                let end = parser.parse(End::parse)?;
                Ok(Some(NamespaceDeclaration { namespace, id, end }))
            } else {
                Ok(None)
            }
        })?;

        parser
            .parse(multi0(Item::parse))
            .map(|items| TranslationUnit {
                namespace_declaration,
                items,
            })
    }
}

fn parse_item<'p, R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<'p, Item<'p>> {
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

fn parse_class<'p, R: Read>(
    visibility: Option<Visibility<'p>>,
    static_tok: Option<Static<'p>>,
    parser: &mut SyntacticParser<'p, R>,
) -> Result<'p, ItemClass<'p>> {
    let abstract_tok = parser.parse_opt::<Abstract>()?;
    let class = parser.parse(Class::parse)?;
    let id = parser.parse(VarId::parse)?;
    let generics = parse_generics(parser)?;

    let extends = if Extends::could_parse(parser)? {
        Some(parser.parse(|p: &mut SyntacticParser<'p, R>| {
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
        Some(parser.parse(|p: &mut SyntacticParser<'p, R>| {
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
        id,
        generics,
        extends,
        implements,
        members,
    })
}

fn parse_generics<'p, R: Read>(
    parser: &mut SyntacticParser<'p, R>,
) -> Result<'p, Option<GenericDeclarations<'p>>> {
    if LBracket::could_parse(parser)? {
        let bounds = parser.parse(|p: &mut SyntacticParser<'p, R>| {
            let lbracket = p.parse(LBracket::parse)?;
            let bounds = p
                .parse(cut(seperated_list1(
                    Comma::parse,
                    |p: &mut SyntacticParser<'p, R>| {
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
    owner: &VarId<'p>,
    parser: &mut SyntacticParser<'p, R>,
) -> Result<'p, ClassMembers<'p>> {
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

fn parse_class_member<'p, R: Read>(
    owner: &VarId<'p>,
    visibility: Option<Visibility<'p>>,
    is_static: Option<Static<'p>>,
    parser: &mut SyntacticParser<'p, R>,
) -> Result<'p, ClassMember<'p>> {
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

fn parse_field<'p, R: Read>(
    owner: &VarId<'p>,
    visibility: Option<Visibility<'p>>,
    is_static: Option<Static<'p>>,
    parser: &mut SyntacticParser<'p, R>,
) -> Result<'p, ClassMember<'p>> {
    let final_tok = parser.parse_opt::<Final>()?;
    let binding = parser.parse(cut(Binding::parse))?;
    let default_value = if let Some(assign) = parser.parse_opt::<Assign>()? {
        let value = parser.parse(cut(Expr::parse))?;
        Some(ClassFieldDefaultValue { assign, value })
    } else {
        None
    };

    let class_field = ClassField {
        vis: visibility,
        static_tok: is_static,
        final_tok,
        binding,
        default_value,
    };

    Ok(ClassMember::Field(class_field))
}

fn parse_method<'p, R: Read>(
    owner: &VarId<'p>,
    visibility: Option<Visibility<'p>>,
    is_static: Option<Static<'p>>,
    parser: &mut SyntacticParser<'p, R>,
) -> Result<'p, ClassMember<'p>> {
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
        let types = parser.parse(Punctuated1::<Type<'p>, Comma<'p>>::parse)?;
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
            body: parser.parse(StatementBlock::parse)?,
        };
        let fn_ = ItemFn {
            vis: visibility,
            static_tok: is_static,
            fn_tok,
            id: name,
            generics,
            parameters,
            fn_return,
            fn_throws,
            body,
        };
        Ok(ClassMember::Method(fn_))
    }
}

fn parse_constructor<'p, R: Read>(
    owner: &VarId<'p>,
    visibility: Option<Visibility<'p>>,
    parser: &mut SyntacticParser<'p, R>,
) -> Result<'p, ClassMember<'p>> {
    let constructor = parser.parse(Constructor::parse)?;
    let generics = parser.parse(parse_generics)?;
    let parameters = parser.parse(cut(FnParameters::parse))?;
    let fn_throws = if Throws::could_parse(parser)? {
        let throws = parser.parse(Throws::parse)?;
        let types = parser.parse(Punctuated1::<Type<'p>, Comma<'p>>::parse)?;
        Some(FnThrows { throws, types })
    } else {
        None
    };

    let body = FnBody {
        body: parser.parse(StatementBlock::parse)?,
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

fn parse_function<'p, R: Read>(
    visibility: Option<Visibility<'p>>,
    parser: &mut SyntacticParser<'p, R>,
) -> Result<'p, ItemFn<'p>> {
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
        let types = parser.parse(Punctuated1::<Type<'p>, Comma<'p>>::parse)?;
        Some(FnThrows { throws, types })
    } else {
        None
    };

    let body = FnBody {
        body: parser.parse(StatementBlock::parse)?,
    };
    let fn_ = ItemFn {
        vis: visibility,
        static_tok: None,
        fn_tok,
        id: name,
        generics,
        parameters,
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
