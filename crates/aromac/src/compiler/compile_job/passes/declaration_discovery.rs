use crate::compiler::compile_job::{CompileJob, CompileJobState, TypeData};
use crate::resolution::TranslationData;
use aroma_ast::items::Item;
use aroma_ast::translation_unit::TranslationUnit;
use aroma_tokens::id::Id;
use aroma_tokens::id_resolver::IdResolver;
use aroma_types::hierarchy::Error;
use std::collections::{HashMap, HashSet};
use thiserror::Error;
use tracing::{debug, instrument, trace, warn};

/// Creates the identifiers for this translation unit
#[instrument(skip_all)]
pub fn create_declarations(
    tu: TranslationUnit,
    data: TranslationData,
) -> Result<CompileJobState, CreateDeclarationError> {
    trace!("tu: {tu:#?}");
    let mut translation_data = data;
    let mut built = HashMap::new();

    for item in &tu.items {
        match item {
            Item::Class(cls) => {
                translation_data.insert_class(&cls.class)?;
                let id = cls.class.id().clone();
                built.insert(id, TypeData::Class(cls.class.clone()));
            }
            Item::Delegate(_) => {}
        }
    }

    trace!("translation_data: {translation_data:#?}");

    Ok(CompileJobState::IdentifiersCreated(tu, translation_data, built))
}

#[derive(Debug, Error)]
pub enum CreateDeclarationError {
    #[error(transparent)]
    HierarchyError(#[from] aroma_types::hierarchy::Error),
}
