use crate::compiler::compile_job::{CompileJob, CompileJobState};
use crate::resolution::TranslationData;
use aroma_ast::items::Item;
use aroma_ast::translation_unit::TranslationUnit;
use aroma_tokens::id::Id;
use aroma_tokens::id_resolver::IdResolver;
use aroma_types::hierarchy::Error;
use log::{debug, warn};
use std::collections::HashSet;
use thiserror::Error;

/// Creates the identifiers for this translation unit
pub fn create_declarations(
    job: &mut CompileJob,
    tu: TranslationUnit,
) -> Result<CompileJobState, CreateIdentifierError> {
    debug!("tu: {tu:#?}");
    let mut translation_data = TranslationData::new();

    for item in &tu.items {
        match item {
            Item::Class(cls) => {
                translation_data.insert_class(&cls.class)?;
            }
        }
    }

    Ok(CompileJobState::IdentifiersCreated(tu, translation_data))
}

#[derive(Debug, Error)]
pub enum CreateIdentifierError {
    #[error(transparent)]
    HierarchyError(#[from] aroma_types::hierarchy::Error),
}
