//! full qualifies everything in the MIR

use std::collections::HashSet;
use crate::compiler::compile_job::{CompileError, CompileJobState};
use crate::resolution::TranslationData;
use aroma_ast::translation_unit::TranslationUnit;
use aroma_tokens::id_resolver::IdResolver;
use log::debug;
use aroma_tokens::id::Id;

/// Attempts to fully qualify a translation unit
pub fn fully_qualify(
    translation_unit: TranslationUnit,
) -> Result<CompileJobState, CompileError> {
    let namespace = translation_unit
        .namespace
        .as_ref()
        .cloned()
        .unwrap_or_default();
    let mut translation_data = TranslationData::new();
    let mut missing = HashSet::<Id>::new();

    for import in &translation_unit.imports {
        translation_data
            .id_resolver_mut()
            .build_namespace(namespace.clone())
            .insert_alias(import.most_specific(), import.clone());
    }

    debug!(
        "starting id fqi conversion process with id resolver: {id_resolver:#?}",
        id_resolver = translation_data.id_resolver()
    );


    todo!()
}
