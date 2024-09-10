use crate::compiler::compile_job::{CompileError, CompileJobState};
use crate::resolution::TranslationData;
use aroma_ast::translation_unit::TranslationUnit;
use aroma_ast::AstVisitorMut;
use aroma_tokens::id::Id;
use aroma_tokens::spanned::Spanned;
use aroma_tokens::SpannedError;
use aroma_types::class::{ClassInst, ClassRef};
use std::collections::HashSet;
use tracing::instrument;

#[derive(Debug)]
pub struct TypeCheckPass {
    tu: TranslationUnit,
    data: TranslationData,
    missing: HashSet<Id>,
}

impl TypeCheckPass {
    pub fn new(tu: TranslationUnit, data: TranslationData) -> Self {
        Self {
            tu,
            data,
            missing: Default::default(),
        }
    }

    #[instrument(skip(self), name = "type_check")]
    pub fn pass(mut self) -> Result<CompileJobState, CompileError> {
        let mut this = match self.detect_missing_status() {
            Ok(this) => this,
            Err(state) => return Ok(state),
        };
        let mut type_checker = TypeChecker { td: &this.data };
        let unit = &mut this.tu;
        type_checker.visit_translation_unit(unit)?;

        //todo: type check
        if this.missing.is_empty() {
            todo!("okay")
        } else {
            let missing_clone = this.missing.clone();
            Ok(CompileJobState::WaitingForIdentifiers(this, missing_clone))
        }
    }

    fn detect_missing_status(mut self) -> Result<Self, CompileJobState> {
        let mut found = HashSet::new();
        for missing in &self.missing {
            if self.data.globals().contains_key(missing)
                || self
                    .data
                    .class_hierarchy()
                    .contains_ref(&ClassRef::from(missing.clone()))
            {
                found.insert(missing.clone());
            }
        }
        self.missing.retain(|i| !found.contains(i));
        if !self.missing.is_empty() {
            let missing_clone = self.missing.clone();
            return Err(CompileJobState::WaitingForIdentifiers(self, missing_clone));
        }
        Ok(self)
    }
}

struct TypeChecker<'a> {
    td: &'a TranslationData,
}

impl AstVisitorMut for TypeChecker<'_> {
    type Err = CompileError;

    fn visit_class_inst(&mut self, id: &mut ClassInst) -> Result<(), Self::Err> {
        let _ = self
            .td
            .class_hierarchy()
            .instantiate(id.class_ref(), id.generics().iter().cloned())
            .map_err(|e| SpannedError::new(e.into(), id.span(), None))?;
        Ok(())
    }
}
