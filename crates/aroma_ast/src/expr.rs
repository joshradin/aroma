//! MIR expression

use aroma_tokens::id::Id;
use crate::block::Block;
use crate::references::{FieldRef, GlobalRef, MethodRef};
use crate::typed::{TypeError, TypeInfo, TypeState, Typed, TypedMut};
use aroma_types::type_signature::TypeSignature;

/// An expression
#[derive(Debug)]
pub enum Expr {
    Global(GlobalExpr),
    Var(VarExpr),
    Field(FieldExpr),
    CallMethod(CallMethodExpr),
    CallFunction(),
    Index(),
    Unary(),
    Binary(),
    Block(Block),
    Closure(),
    Ternary(),
    This(ExprThis),
    Super,
    As(),
}

impl Typed<TypeSignature> for Expr {
    fn get_type(&self) -> TypeState<TypeSignature, TypeError> {
        match self {
            Expr::Field(f) => f.get_type().to_signature(),
            Expr::Var(f) => f.get_type().to_signature(),
            Expr::Global(f) => f.get_type().to_signature(),
            Expr::CallMethod(m) => m.get_type().to_signature(),
            _ => {
                todo!()
            }
        }
    }
}

/// Get a field from a type
#[derive(Debug)]
pub struct FieldExpr {
    pub object: Box<Expr>,
    pub field: String,
    field_ref: TypeState<FieldRef>,
}

impl FieldExpr {
    pub fn new(object: Expr, field: String) -> Self {
        Self {
            object: Box::new(object),
            field,
            field_ref: TypeState::default(),
        }
    }

    pub fn with_field_ref(object: Expr, field: String, ty: impl Into<TypeState<FieldRef>>) -> Self {
        Self {
            object: Box::new(object),
            field,
            field_ref: ty.into(),
        }
    }
}

impl Typed<FieldRef> for FieldExpr {
    fn get_type(&self) -> TypeState<FieldRef, TypeError> {
        self.field_ref.clone()
    }
}

impl TypedMut<FieldRef> for FieldExpr {
    fn get_type_mut(&mut self) -> &mut TypeState<FieldRef, TypeError> {
        &mut self.field_ref
    }
}

/// Call a method
#[derive(Debug)]
pub struct CallMethodExpr {
    pub object: Box<Expr>,
    pub parameters: Vec<Expr>,
    method_ref: TypeState<MethodRef>,
}

impl CallMethodExpr {
    pub fn new(object: Expr, parameters: impl IntoIterator<Item = Expr>) -> Self {
        Self {
            object: Box::new(object),
            parameters: parameters.into_iter().collect(),
            method_ref: TypeState::default(),
        }
    }
}

impl Typed<MethodRef> for CallMethodExpr {
    fn get_type(&self) -> TypeState<MethodRef, TypeError> {
        self.method_ref.clone()
    }
}

impl TypedMut<MethodRef> for CallMethodExpr {
    fn get_type_mut(&mut self) -> &mut TypeState<MethodRef, TypeError> {
        &mut self.method_ref
    }
}

/// A global variable expression
#[derive(Debug)]
pub struct GlobalExpr {
    pub id: Id,
    global_ref: TypeState<GlobalRef>,
}

impl GlobalExpr {
    pub fn new(id: Id) -> Self {
        Self {
            id,
            global_ref: TypeState::default(),
        }
    }
}

impl Typed<GlobalRef> for GlobalExpr {
    fn get_type(&self) -> TypeState<GlobalRef, TypeError> {
        self.global_ref.clone()
    }
}

impl TypedMut<GlobalRef> for GlobalExpr {
    fn get_type_mut(&mut self) -> &mut TypeState<GlobalRef, TypeError> {
        &mut self.global_ref
    }
}

/// A variable expression
#[derive(Debug)]
pub struct VarExpr {
    pub id: String,
    state: TypeState<TypeSignature>,
}

impl VarExpr {
    pub fn new(id: String) -> Self {
        Self {
            id,
            state: TypeState::default(),
        }
    }
}

impl Typed<TypeSignature> for VarExpr {
    fn get_type(&self) -> TypeState<TypeSignature> {
        self.state.clone()
    }
}

impl TypedMut<TypeSignature> for VarExpr {
    fn get_type_mut(&mut self) -> &mut TypeState<TypeSignature> {
        &mut self.state
    }
}

/// A variable expression
#[derive(Debug, Default)]
pub struct ExprThis {
    state: TypeState<TypeSignature>,
}

impl ExprThis {
    pub fn new(ty: impl Into<TypeState<TypeSignature>>) -> Self {
        Self { state: ty.into() }
    }
}

impl Typed<TypeSignature> for ExprThis {
    fn get_type(&self) -> TypeState<TypeSignature> {
        self.state.clone()
    }
}

impl TypedMut<TypeSignature> for ExprThis {
    fn get_type_mut(&mut self) -> &mut TypeState<TypeSignature> {
        &mut self.state
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::{Expr, ExprThis, FieldExpr};
    use crate::typed::Typed;

    #[test]
    fn test_type_of_expr() {
        let expr = Expr::Field(FieldExpr::new(
            Expr::This(ExprThis::default()),
            "field".to_string(),
        ))
        .get_type();
    }
}
