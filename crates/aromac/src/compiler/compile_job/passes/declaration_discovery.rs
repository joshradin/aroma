use crate::compiler::compile_job::{CompileJob, CompileJobState};
use aroma_ast::mir::translation_unit::TranslationUnit;
use log::debug;
use thiserror::Error;

/// Creates the identifiers for this
pub fn find_declarations(
    job: &mut CompileJob,
    tu: TranslationUnit,
) -> Result<CompileJobState, CreateIdentifierError> {
    debug!("tu: {tu:#?}");
    todo!()
}

#[derive(Debug, Error)]
pub enum CreateIdentifierError {}
