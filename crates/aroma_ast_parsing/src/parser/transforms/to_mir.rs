use crate::parser::hir::items as parser_items;
use crate::parser::hir::items::ItemFn;
use crate::parser::hir::items::Visibility;
use crate::parser::hir::items::{ClassField, ClassMember};
use crate::parser::hir::singletons::Static;
use crate::parser::hir::translation_unit::TranslationUnit as ParsedTranslationUnit;
use crate::parser::hir::Punctuated;
use crate::parser::SyntaxError;
use aroma_ast::items::{ClassItem, Item};
use aroma_ast::method::MethodDef;
use aroma_ast::translation_unit::TranslationUnit;
use aroma_tokens::id::Id;
use aroma_tokens::id_resolver::IdResolver;
use aroma_tokens::spanned::Spanned;
use aroma_types::class::{AsClassRef, Class, ClassInst, ClassKind, ClassRef};
use aroma_types::field::Field;
use aroma_types::functions::{FunctionDeclaration, Parameter};
use aroma_types::generic::GenericDeclaration;
use aroma_types::hierarchy::intrinsics::OBJECT_CLASS;
use aroma_types::vis::Vis;
use method_hir_to_mir::method_hir_to_mir_def;
use std::collections::HashMap;
use tracing::{debug, trace};

mod class_hir_to_mir;
mod expr_hir_to_mir;
mod interface_hir_to_mir;
mod method_hir_to_mir;
mod constructor_hir_to_mir;

/// Runs the initial conversion to mir, removing most tokens while keeping spans
pub fn to_mir(translation_unit: ParsedTranslationUnit) -> Result<TranslationUnit, SyntaxError> {
    let span = translation_unit.span();
    let namespace = translation_unit
        .namespace_declaration
        .as_ref()
        .map(|i| &i.id);

    let imports: Vec<Id> = translation_unit
        .imports
        .iter()
        .map(|import| import.id.clone())
        .collect();

    let items = translation_unit.items.into_iter().try_fold(
        Vec::new(),
        |mut items, item| -> Result<Vec<Item>, SyntaxError> {
            match item {
                parser_items::Item::Class(cls) => {
                    let class = class_hir_to_mir::class_hir_to_mir(namespace, cls)?;
                    trace!("parsed class {class:#?}");
                    items.push(Item::Class(class));
                }
                parser_items::Item::Func(_func) => {}
                parser_items::Item::Interface(interface) => {
                    let interface =
                        interface_hir_to_mir::interface_hir_to_mir(namespace, interface)?;
                    trace!("parsed interface {interface:#?}");
                    items.push(Item::Class(interface));
                }
            }
            Ok(items)
        },
    )?;

    let mir = TranslationUnit::new(
        span,
        translation_unit.namespace_declaration.map(|i| i.id),
        imports,
        items,
    );
    Ok(mir)
}

pub fn vis_hir_to_mir(option: Option<Visibility>) -> Vis {
    option.as_ref().map(|v| Vis::from(v)).unwrap_or_default()
}
