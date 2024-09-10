use crate::compiler::compile_job::{CompileError, CompileJobState};
use crate::resolution::TranslationData;
use aroma_ast::translation_unit::TranslationUnit;
use aroma_ast::AstVisitorMut;
use aroma_tokens::id::Id;
use aroma_types::class::{ClassInst, ClassRef};
use aroma_types::hierarchy::Error;
use std::collections::HashSet;
use tracing::{debug, instrument, warn};

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
        let mut type_checker = TypeChecker {
            td: &this.data,
            missing: &mut this.missing,
        };
        let unit = &mut this.tu;
        type_checker.visit_translation_unit(unit)?;

        if this.missing.is_empty() {
            Ok(CompileJobState::TypesChecked(this.tu, this.data))
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

    pub fn data(&self) -> &TranslationData {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut TranslationData {
        &mut self.data
    }
}

struct TypeChecker<'a> {
    td: &'a TranslationData,
    missing: &'a mut HashSet<Id>,
}

impl AstVisitorMut for TypeChecker<'_> {
    type Err = CompileError;

    fn visit_class_inst(&mut self, id: &mut ClassInst) -> Result<(), Self::Err> {
        match self
            .td
            .class_hierarchy()
            .instantiate(id.class_ref(), id.generics().iter().cloned())
        {
            Ok(_) => {}
            Err(e) => match e {
                Error::ClassNotDefined(not_defined) => {
                    warn!("class not defined: {not_defined}");
                    self.missing.insert(not_defined.as_ref().clone());
                }
                _ => {}
            },
        }
        Ok(())
    }
}
