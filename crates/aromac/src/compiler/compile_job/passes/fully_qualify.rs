//! full qualifies everything in the MIR

use crate::compiler::compile_job::{CompileError, CompileErrorKind, CompileJobState};
use crate::resolution::TranslationData;
use aroma_ast::items::ClassItem;
use aroma_ast::translation_unit::TranslationUnit;
use aroma_ast::AstVisitorMut;
use aroma_tokens::id::Id;
use aroma_tokens::id_resolver::{IdError, IdQueries, IdResolver, NamespaceBuilder, ResolveIdError};
use aroma_tokens::spanned::Spanned;
use aroma_tokens::SpannedError;
use aroma_types::class::{Class, ClassInst, ClassRef};
use aroma_types::functions::FunctionDeclaration;
use aroma_types::generic::GenericDeclaration;
use itertools::Itertools;
use log::{debug, error, warn};
use std::collections::HashSet;

/// Attempts to fully qualify a translation unit
pub fn fully_qualify(
    mut translation_unit: TranslationUnit,
) -> Result<CompileJobState, CompileError> {
    let namespace = translation_unit
        .namespace
        .as_ref()
        .cloned()
        .unwrap_or_default();
    let mut translation_data = TranslationData::new();

    for import in &translation_unit.imports {
        translation_data
            .id_resolver_mut()
            .build_namespace(namespace.clone())
            .insert_alias(import.most_specific(), import.clone())?;
    }

    debug!(
        "starting id fqi conversion process with id resolver: {id_resolver:#?}",
        id_resolver = translation_data.id_resolver()
    );

    let builder = translation_data
        .id_resolver_mut()
        .build_namespace(namespace.clone());
    let mut full_qualifier = FullQualifier {
        builder,
        ignores: vec![],
        missing: HashSet::new(),
    };
    full_qualifier.visit_translation_unit(&mut translation_unit)?;

    debug!("post-qualified: {translation_unit:#?}");
    let missing = full_qualifier.missing;
    if !missing.is_empty() {
        error!("missing identifiers: {}", missing.iter().join(", "));
        return Err(CompileErrorKind::UndefinedIdentifiers(Vec::from_iter(missing)).into());
    }

    todo!()
}

#[derive(Debug)]
struct FullQualifier<'a> {
    builder: NamespaceBuilder<'a>,
    ignores: Vec<ClassInst>,
    missing: HashSet<Id>,
}

impl<'a> FullQualifier<'a> {
    fn with_generics<F: FnOnce(&mut Self) -> R, R>(
        &mut self,
        generics: Vec<GenericDeclaration>,
        cb: F,
    ) -> R {
        debug!("setting ignores to {:?}", generics);
        let len = self.ignores.len();
        self.ignores
            .extend(generics.iter().map(|gen| ClassInst::from(gen.id())));
        let r = cb(self);
        self.ignores.drain(len..);
        r
    }
}

impl AstVisitorMut for FullQualifier<'_> {
    type Err = CompileError;

    fn visit_class_item(&mut self, class: &mut ClassItem) -> Result<(), Self::Err> {
        let x = Vec::from(class.class.generics());
        self.with_generics(x, |visitor| {
            visitor.defaults().visit_class_item(visitor, class)
        })
    }

    fn visit_class(&mut self, class: &mut Class) -> Result<(), Self::Err> {
        let id = class.id();
        self.builder
            .insert_alias(id.most_specific(), id.clone())
            .map_err(|e| CompileError::new(e, id.span(), None))?;
        self.defaults().visit_class(self, class)?;
        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        func_dec: &mut FunctionDeclaration,
    ) -> Result<(), Self::Err> {
        let generics = Vec::from(func_dec.generic_declarations());
        self.with_generics(generics, |visitor| {
            visitor
                .defaults()
                .visit_function_declaration(visitor, func_dec)
        })
    }

    fn visit_class_inst(&mut self, id: &mut ClassInst) -> Result<(), Self::Err> {
        if self.ignores.contains(id) {
            Ok(())
        } else {
            self.defaults().visit_class_inst(self, id)
        }
    }

    fn visit_class_ref(&mut self, id: &mut ClassRef) -> Result<(), Self::Err> {
        match self.builder.query().resolve(id.as_ref()) {
            Ok(resolved) => {
                if resolved != id.as_ref() {
                    debug!("fully qualified {id} to {resolved}");
                    *id.as_mut() = resolved.clone();
                }
                Ok(())
            }
            Err(ResolveIdError::NotFound {
                namespace: _,
                query,
            }) => {
                self.missing.insert(query);
                Ok(())
            }
            Err(e) => Err(CompileError::new(e, id.as_ref().span(), None)),
        }
    }
}
