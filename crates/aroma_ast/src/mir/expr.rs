//! MIR expression

use crate::mir::references::{FieldRef, MethodRef};
use crate::mir::typed::{TypeError, TypeState, Typed};

/// An expression
#[derive(Debug)]
pub enum Expr {
    Field(ExprField),
    CallMethod(ExprMethodCall),
    CallFunction(),
}

/// Get a field from a type
#[derive(Debug)]
pub struct ExprField {
    pub object: Box<Expr>,
    pub field_ref: TypeState<FieldRef>,
}

impl Typed<FieldRef> for ExprField {
    fn get_type(&self) -> TypeState<&FieldRef, &TypeError> {
        self.field_ref.as_ref()
    }
}

/// Get a field from a type
#[derive(Debug)]
pub struct ExprMethodCall {
    pub object: Box<Expr>,
    pub parameters: Vec<Expr>,
    pub method_ref: TypeState<MethodRef>,
}

impl Typed<MethodRef> for ExprMethodCall {
    fn get_type(&self) -> TypeState<&MethodRef, &TypeError> {
        self.method_ref.as_ref()
    }
}
