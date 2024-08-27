use std::collections::HashSet;
use crate::compiler::compile_job::{CompileJob, CompileJobState};
use aroma_ast::mir::translation_unit::TranslationUnit;
use log::debug;
use thiserror::Error;
use aroma_ast::id::Id;
use aroma_ast::mir::items::Item;
use crate::resolution::TranslationData;

/// Creates the identifiers for this translation unit
pub fn find_declarations(
    job: &mut CompileJob,
    tu: TranslationUnit,
) -> Result<CompileJobState, CreateIdentifierError> {
    debug!("tu: {tu:#?}");
    let mut translation_data =
        TranslationData::new();
    let mut missing = HashSet::new();
    for item in &tu.items {
        match item {
            Item::Class(cls) => {
                translation_data.class_hierarchy_mut()
                    .insert(
                        cls.class.clone()
                    )?;
            }
        }
    }

    debug!("data state: {:#?}", translation_data);
    if missing.is_empty() {
        Ok(CompileJobState::IdentifiersCreated(
            tu, translation_data
        ))
    } else {
        Ok(CompileJobState::WaitingForIdentifiers(
            tu,
            missing,
            translation_data,
        ))
    }
}

#[derive(Debug, Error)]
pub enum CreateIdentifierError {
    #[error(transparent)]
    HierarchyError(#[from] aroma_types::hierarchy::Error)
}
