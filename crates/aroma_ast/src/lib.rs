#![doc = include_str!("../README.md")]

use aroma_visitor_gen::visitor;
use crate::translation_unit::TranslationUnit;

pub mod block;
pub mod expr;
pub mod items;
pub mod method;
pub mod references;
pub mod statements;
pub mod translation_unit;
pub mod typed;

visitor! {
    /// The AST visitor trait, used for visiting aroma blocks
    pub trait AstVisitor(Ref: _, Mut) {
        visit fn translation_unit(v, tu: TranslationUnit) -> Result<()> {

            Ok(())
        }


    }
}