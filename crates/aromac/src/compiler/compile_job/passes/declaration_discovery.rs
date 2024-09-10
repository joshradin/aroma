use crate::compiler::compile_job::{CompileJob, CompileJobState};
use crate::resolution::TranslationData;
use aroma_ast::items::Item;
use aroma_ast::translation_unit::TranslationUnit;
use aroma_tokens::id::Id;
use aroma_tokens::id_resolver::IdResolver;
use aroma_types::hierarchy::Error;
use std::collections::HashSet;
use thiserror::Error;
use tracing::{debug, instrument, warn};

/// Creates the identifiers for this translation unit
#[instrument(skip_all)]
pub fn create_declarations(
    tu: TranslationUnit,
    data: TranslationData,
) -> Result<CompileJobState, CreateDeclarationError> {
    debug!("tu: {tu:#?}");
    let mut translation_data = data;

    for item in &tu.items {
        match item {
            Item::Class(cls) => {
                translation_data.insert_class(&cls.class)?;
            }
            Item::Delegate(_) => {}
        }
    }

    debug!("translation_data: {translation_data:#?}");

    Ok(CompileJobState::IdentifiersCreated(tu, translation_data))
}

#[derive(Debug, Error)]
pub enum CreateDeclarationError {
    #[error(transparent)]
    HierarchyError(#[from] aroma_types::hierarchy::Error),
}
