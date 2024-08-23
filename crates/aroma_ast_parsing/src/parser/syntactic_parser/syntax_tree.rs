//! Syntax tree

use crate::parser::syntactic_parser::error::{ErrorKind, SyntaxError};
use crate::parser::SyntacticParser;
use aroma_ast::token::ToTokens;
use std::fmt::Debug;
use std::io::Read;
use aroma_ast::id::Id;
use aroma_visitor_gen::visitor;

pub mod binding;
pub mod expr;
mod helpers;
pub mod items;
pub mod singletons;
pub mod statement;
pub mod constants;

use crate::parser::syntactic_parser::{Err, Parsable};
pub use helpers::*;

use items::*;
use crate::parser::singletons::VarId;

visitor! {
    pub trait SyntaxTreeVisitor {
        visit fn translation_unit(v, unit: &TranslationUnit) -> Result<()> {
            if let Some(nd) = &unit.namespace_declaration {
                v.visit_namespace_declaration(nd)?;
            }
            for item in &unit.items {
                v.visit_item(item)?;
            }
            Ok(())
        }

        visit fn namespace_declaration(v, nd: &NamespaceDeclaration) -> Result<()> {
            v.visit_id(&nd.id)?;
            Ok(())
        }

        visit fn item(v, item: &Item) -> Result<()> {
            match item {
                Item::Class(cls) => { v.visit_class(cls)}
                Item::Func(func) => { todo!()}
            }
        }

        visit fn class(v, class: &ItemClass) -> Result<()> {

            Ok(())
        }

        visit fn id(v, id: &Id) -> Result<()> {
            Ok(())
        }

        visit fn var_id(v, id: &VarId) -> Result<()> {
            v.visit_id(&id.id)
        }
    }
}