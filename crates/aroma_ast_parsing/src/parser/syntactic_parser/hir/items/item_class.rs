use crate::parser::annotation::Annotation;
use crate::parser::binding::{Binding, FnParameters, Type};
use crate::parser::expr::Expr;
use crate::parser::items::{GenericDeclaration, GenericDeclarations, Visibility};
use crate::parser::singletons::{
    Abstract, Arrow, Assign, Class, Comma, Constructor, Extends, Final, Implements, LBracket,
    LCurly, RBracket, RCurly, Static, Throws, VarId,
};
use crate::parser::statement::BlockStatement;
use crate::parser::syntactic_parser::hir::items::item_function::ItemFn;
use crate::parser::{
    cut, seperated_list1, singletons, CouldParse, End, ErrorKind, Parsable, Punctuated1,
    SyntacticParser, SyntaxResult,
};
use aroma_tokens::token::{ToTokens, TokenKind};
use tracing::{debug, trace};
use std::io::Read;

/// A class declaration
#[derive(Debug, ToTokens)]
pub struct ItemClass {
    pub annotations: Vec<Annotation>,
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
    pub fn_tok: singletons::Fn,
    pub ident: VarId,
    pub generics: Option<GenericDeclarations>,
    pub fn_parameters: FnParameters,
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

pub fn parse_class<R: Read>(
    annotations: Vec<Annotation>,
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
        annotations,
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

pub fn parse_generics<'p, R: Read>(
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
            let annotations = parser.parse(Vec::<Annotation>::parse)?;
            let visibility = parser.parse_opt::<Visibility>()?;
            let is_static = parser.parse_opt::<Static>()?;
            let member = parse_class_member(annotations, owner, visibility, is_static, parser)?;
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
    annotations: Vec<Annotation>,
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
            parse_field(annotations, owner, visibility, is_static, parser)
        }
        TokenKind::Constructor if is_static.is_none() => {
            parse_constructor(annotations, owner, visibility, parser)
        }
        TokenKind::Constructor if is_static.is_some() => {
            Err(parser.error(ErrorKind::ConstructorsCanNotBeStatic, None))
        }
        TokenKind::Fn | TokenKind::Abstract => {
            parse_method(annotations, owner, visibility, is_static, parser)
        }
        _ => Err(parser.error(
            ErrorKind::expected_token(["fn", "class", "id", "constructor"], lookahead),
            None,
        )),
    }
}

fn parse_field<R: Read>(
    annotations: Vec<Annotation>,
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
    annotations: Vec<Annotation>,
    owner: &VarId,
    visibility: Option<Visibility>,
    is_static: Option<Static>,
    parser: &mut SyntacticParser<'_, R>,
) -> SyntaxResult<ClassMember> {
    let abstract_tok = parser.parse_opt::<Abstract>()?;
    if abstract_tok.is_some() && is_static.is_some() {
        return Err(parser.error("Can not abstract static methods", None));
    }
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

    if let Some(abstract_tok) = abstract_tok {
        let end = parser.parse(End::parse)?;
        let abstract_fn = ItemAbstractFn {
            vis: visibility,
            abstract_tok,
            fn_tok,
            ident: name,
            generics,
            fn_parameters: parameters,
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
            annotations,
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
    annotations: Vec<Annotation>,
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
