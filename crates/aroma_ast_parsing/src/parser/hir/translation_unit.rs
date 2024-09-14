
use crate::parser::hir::items::Item;
use crate::parser::hir::singletons::{Import, Namespace};
use crate::parser::hir::{cut, multi0, End, SyntaxError};
use aroma_tokens::id::Id;
use aroma_tokens::token::ToTokens;
use std::io::Read;
use crate::parser::blocking::{remove_nl, BlockingParser};
use crate::parser::SyntaxResult;
use crate::parser::hir_parser::blocking::Parsable;

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

impl TranslationUnit {
    /// Checks if this translation unit has any items
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

impl Parsable for TranslationUnit {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut BlockingParser<'_, R>) -> SyntaxResult<Self> {
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

        let imports = parser.parse(multi0(|parser: &mut BlockingParser<'_, R>| {
            parser.with_ignore_nl(false, |parser: &mut BlockingParser<'_, R>| {
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
