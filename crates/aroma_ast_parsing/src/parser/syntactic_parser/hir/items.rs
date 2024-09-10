//! items, like functions, and classes

use crate::parser::annotation::Annotation;
use crate::parser::binding::{FnParameters, Type};
use crate::parser::expr::remove_nl;
use crate::parser::singletons::{Private, Protected, Public};
use crate::parser::syntactic_parser::hir::helpers::End;
use crate::parser::{
    cut, singletons::*, CouldParse, ErrorKind, Parsable, Punctuated1, SyntacticParser, SyntaxError,
    SyntaxResult,
};
use aroma_tokens::spanned::Spanned;
use aroma_tokens::token::{ToTokens, TokenKind};
use aroma_types::class::AsClassRef;
use aroma_types::hierarchy::intrinsics::OBJECT_CLASS;
use aroma_types::vis::Vis;
use log::{debug, trace};
use std::io::Read;

mod item_class;
mod item_function;
mod item_interface;

pub use self::{item_class::*, item_function::*, item_interface::*};

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
    pub dot: Dot,
}

/// An item (interface, class, function) declaration, along with its visibility
#[derive(Debug, ToTokens)]
pub enum Item {
    Class(ItemClass),
    Func(ItemFn),
    Interface(ItemInterface),
}

impl Parsable for Item {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
        parse_item(parser)
    }
}

fn parse_item<'p, R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Item> {
    parser.parse(remove_nl)?;
    let annotations =
        parser.with_ignore_nl(true, |parser| parser.parse(cut(Vec::<Annotation>::parse)))?;
    trace!("annotations {:?}", annotations);
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
            let class = parse_class(annotations, vis, None, parser).map_err(|e| e.cut())?;
            Item::Class(class)
        }
        TokenKind::Interface => {
            let interface = parse_interface(vis, parser).map_err(|e| e.cut())?;
            Item::Interface(interface)
        }
        TokenKind::Fn => {
            let func = parse_function(annotations, vis, parser).map_err(|e| e.cut())?;
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
