use crate::mir::expr::Expr;
use crate::mir::typed::{TypeError, TypeState, Typed, TypedMut};
use crate::spanned::{Span, Spanned};
use aroma_types::type_signature::TypeSignature;

/// A statement
#[derive(Debug)]
pub struct Stmt {
    span: Span,
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(span: Span, kind: StmtKind) -> Self {
        Self { span, kind }
    }
}

impl Spanned for Stmt {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

/// Statement kind
#[derive(Debug)]
pub enum StmtKind {
    Declare(DeclareStmt),
    Return(ReturnStmt),
}

/// A declare statement
#[derive(Debug)]
pub struct DeclareStmt {
    pub var_name: String,
    pub allow_reassign: bool,
    pub initial_value: Option<Expr>,
    ty: TypeState<TypeSignature>,
}

impl DeclareStmt {
    pub fn new(
        var_name: impl AsRef<str>,
        allow_reassign: bool,
        ty: impl Into<Option<TypeSignature>>,
        initial_value: impl Into<Option<Expr>>
    ) -> Self {
        Self {
            var_name: var_name.as_ref().to_string(),
            allow_reassign,
            initial_value: initial_value.into(),
            ty: TypeState::from(ty.into()),
        }
    }
}

impl Typed<TypeSignature> for DeclareStmt {
    fn get_type(&self) -> TypeState<TypeSignature, TypeError> {
        self.ty.clone()
    }
}

impl TypedMut<TypeSignature> for DeclareStmt {
    fn get_type_mut(&mut self) -> &mut TypeState<TypeSignature, TypeError> {
        &mut self.ty
    }
}

/// Return from a function
#[derive(Debug)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

impl ReturnStmt {
    pub fn new(value: impl Into<Option<Expr>>) -> Self {
        Self { value: value.into()}
    }
}

impl Typed<TypeSignature> for ReturnStmt {
    fn get_type(&self) -> TypeState<TypeSignature, TypeError> {
        TypeState::never()
    }
}

