//! Contains MIR level method definition

use crate::mir::block::Block;
use crate::mir::references::{GetInfoTypeRef, MethodRef, NameType};
use crate::spanned::Span;
use aroma_types::class::ClassInst;
use aroma_types::type_signature::TypeSignature;

/// A mir method, containing information that's only useful for compiling
#[derive(Debug)]
pub struct MethodDef {
    span: Span,
    parent: ClassInst,
    pub name: String,
    pub parameters: Vec<TypeSignature>,
    pub return_ty: TypeSignature,
    pub throws: Vec<TypeSignature>,
    pub body: Block,
}

impl MethodDef {
    pub fn new(
        span: Span,
        parent: ClassInst,
        name: String,
        parameters: Vec<TypeSignature>,
        return_ty: TypeSignature,
        throws: Vec<TypeSignature>,
        body: Block,
    ) -> Self {
        Self {
            span,
            parent,
            name,
            parameters,
            return_ty,
            throws,
            body,
        }
    }
}

impl GetInfoTypeRef<NameType> for MethodDef {
    fn get_info_type_ref(&self) -> NameType {
        NameType(
            self.name.clone(),
            TypeSignature::Function(self.parameters.clone(), Box::new(self.return_ty.clone())),
        )
    }
}

impl GetInfoTypeRef<MethodRef> for MethodDef {
    fn get_info_type_ref(&self) -> MethodRef {
        MethodRef(self.parent.clone(), self.get_info_type_ref())
    }
}
