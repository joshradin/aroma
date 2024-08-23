use std::collections::HashSet;
use crate::compiler::compile_job::{CompileJob, CompileJobState};
use aroma_ast_parsing::parser::items::{NamespaceDeclaration, TranslationUnit};
use thiserror::Error;
use aroma_ast::id::Id;
use aroma_ast_parsing::parser::{SyntaxTreeVisitor, SyntaxTreeVisitorFunctions};

/// Creates the identifiers for this
pub fn create_identifiers<'p>(job: &mut CompileJob<'p>, tu: TranslationUnit<'p>) -> Result<CompileJobState<'p>, CreateIdentifierError> {
    let mut creator = IdCreator { namespace: None, ids: HashSet::new() };
    creator.visit_translation_unit(&tu)?;
    let ids = creator.ids;
    todo!()
}

#[derive(Debug)]
struct IdCreator<'p> {
    namespace: Option<Id<'p>>,
    ids: HashSet<Id<'p>>
}

impl<'p> SyntaxTreeVisitor for IdCreator<'p> {
    type Err = CreateIdentifierError;

    fn visit_namespace_declaration(&mut self, nd: &NamespaceDeclaration) -> Result<(), Self::Err> {
        let id = &nd.id;
        self.namespace = Some(id.clone());
        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum CreateIdentifierError {
}