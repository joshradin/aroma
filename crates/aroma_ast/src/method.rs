//! Contains MIR level method definition

use crate::block::Block;
use crate::references::{GetInfoTypeRef, MethodRef, NameType};
use aroma_tokens::spanned::Span;
use aroma_types::class::ClassInst;
use aroma_types::functions::{FunctionDeclaration, Parameter};
use aroma_types::generic::GenericDeclaration;
use aroma_types::type_signature::TypeSignature;

/// A mir method, containing information that's only useful for compiling
#[derive(Debug)]
pub struct MethodDef {
    span: Span,
    this: Option<ClassInst>,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub generic_declaration: Vec<GenericDeclaration>,
    pub return_ty: TypeSignature,
    pub throws: Vec<ClassInst>,
    pub body: Block,
}

impl MethodDef {
    pub fn new(
        span: Span,
        parent: Option<&ClassInst>,
        dec: &FunctionDeclaration,
        body: Block,
    ) -> Self {
        Self {
            span,
            this: parent.cloned(),
            name: dec.name().to_string(),
            parameters: Vec::from(dec.parameters()),
            generic_declaration: Vec::from(dec.generic_declarations()),
            return_ty: dec
                .return_type()
                .cloned()
                .map(|inst| TypeSignature::from(inst))
                .unwrap_or(TypeSignature::Void),
            throws: Vec::from(dec.throws()),
            body,
        }
    }
}

impl GetInfoTypeRef<NameType> for MethodDef {
    fn get_info_type_ref(&self) -> NameType {
        NameType(
            self.name.clone(),
            TypeSignature::Function(
                self.parameters
                    .iter()
                    .map(|i| i.class.clone().into())
                    .collect(),
                Box::new(self.return_ty.clone()),
            ),
        )
    }
}

impl GetInfoTypeRef<MethodRef> for MethodDef {
    fn get_info_type_ref(&self) -> MethodRef {
        MethodRef(
            self.this.clone().expect("static method"),
            self.get_info_type_ref(),
        )
    }
}
