//! Syntax tree

use crate::parser::syntactic_parser::error::{ErrorKind, SyntaxError};
use crate::parser::SyntacticParser;
use aroma_ast::id::Id;
use aroma_ast::token::ToTokens;
use aroma_visitor_gen::visitor;
use std::fmt::Debug;
use std::io::Read;

pub mod binding;
pub mod constants;
pub mod expr;
mod helpers;
pub mod items;
pub mod singletons;
pub mod statement;
pub mod translation_unit;

use crate::parser::syntactic_parser::{Err, Parsable};
pub use helpers::*;

use crate::parser::singletons::VarId;
use crate::parser::translation_unit::{NamespaceDeclaration, TranslationUnit};
use items::*;

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
                Item::Func(func) => { v.visit_func(func)}
            }
        }

        visit fn id(v, id: &Id) -> Result<()> {
            Ok(())
        }

        visit fn var_id(v, var_id: &VarId) -> Result<()> {
            v.visit_id(&var_id.id)?;
            Ok(())
        }

        visit fn class(v, class: &ItemClass) -> Result<()> {
            v.visit_var_id(&class.ident)?;
            class.members.members.iter().try_for_each(|member| {
                v.visit_class_member(member)
            })?;
            Ok(())
        }

        visit fn class_member(v, class_member: &ClassMember) -> Result<()> {
            Ok(())
        }

        visit fn func(v, func: &ItemFn) -> Result<()> {

            Ok(())
        }
    }
}
