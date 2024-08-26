use crate::parser::expr::remove_nl;
use crate::parser::items::Item;
use crate::parser::singletons::{Import, Namespace};
use crate::parser::{cut, multi0, End, Parsable, SyntacticParser, SyntaxError, SyntaxResult};
use aroma_ast::id::Id;
use aroma_ast::token::ToTokens;
use std::io::Read;

/// Declares the current namespace
#[derive(Debug, ToTokens)]
pub struct NamespaceDeclaration {
    pub namespace: Namespace,
    pub id: Id,
    pub end: End,
}

/// An import
#[derive(Debug, ToTokens)]
pub struct ImportId {
    pub import: Import,
    pub id: Id,
    pub end: End,
}

/// Highest level of parsing, the translation unit consists of items
#[derive(Debug, ToTokens)]
pub struct TranslationUnit {
    pub namespace_declaration: Option<NamespaceDeclaration>,
    pub imports: Vec<ImportId>,
    pub items: Vec<Item>,
}

impl Parsable for TranslationUnit {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
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

        let imports = parser.parse(multi0(|parser: &mut SyntacticParser<'_, R>| {
            parser.with_ignore_nl(false, |parser: &mut SyntacticParser<'_, R>| {
                let import = parser.parse(Import::parse)?;
                let id = parser.parse(Id::parse)?;
                let end = parser.parse(End::parse)?;
                Ok(ImportId { import, id, end })
            })
        }))?;

        parser
            .parse(multi0(Item::parse))
            .map(|items| TranslationUnit {
                namespace_declaration,
                imports,
                items,
            })
    }
}
