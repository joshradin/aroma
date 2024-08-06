use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, BitAnd, BitOr, Div, Mul, Neg, Sub};
use std::sync::Arc;
use itertools::Itertools;
use function::ObjFunction;
pub use obj::Obj;

use crate::types::function::ObjNative;
use crate::vm::error::VmError;

pub mod function;
mod obj;

/// A signature for a function
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnSignature {
    input: Box<[Type]>,
    output: Option<Box<Type>>,
}

impl FnSignature {
    pub fn parameters(&self) -> &[Type] {
        &self.input
    }

    pub fn ret(&self) -> Option<&Type> {
        self.output.as_ref().map(|s| &**s)
    }
}

impl Display for FnSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;
        write!(f, "{}", self.parameters().iter().map(|s| format!("{s}")).join(", "))?;
        write!(f, ")")?;
        if let Some(ret) = self.ret() {
            write!(f, " -> {ret}")?;
        }
        Ok(())
    }
}

/// Every value has a type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Object { signature: String },
    Long,
    Int,
    Char,
    Boolean,
    Byte,
    String,
    Double,
    Float,
    Function(FnSignature),
    NativeFn,
    Never,
}

impl Type {
    pub fn as_fn_signature(&self) -> Option<&FnSignature> {
        if let Type::Function(f) = self {
            Some(f)
        } else {
            None
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Object { signature } => {
                write!(f, "obj<{signature}>")
            }
            Type::Long => {
                write!(f, "i64")
            }
            Type::Int => {
                write!(f, "i32")
            }
            Type::Char => {
                write!(f, "char")
            }
            Type::Boolean => {
                write!(f, "boolean")
            }
            Type::Byte => {
                write!(f, "i8")
            }
            Type::String => {
                write!(f, "str")
            }
            Type::Double => {
                write!(f, "f64")
            }
            Type::Float => {
                write!(f, "f32")
            }
            Type::Function(func) => {
                write!(f, "{func}")
            }
            Type::NativeFn => {
                write!(f, "<native>")
            }
            Type::Never => {
                write!(f, "never")
            }
        }
    }
}

/// A value that can be stored in a stack frame
#[derive(Debug, Clone, PartialEq, PartialOrd, derive_more::TryInto)]
pub enum Value {
    Object(()),
    Long(i64),
    Int(i32),
    Char(char),
    Boolean(bool),
    Byte(i8),
    String(String),
    Double(f64),
    Float(f32),
    Function(Arc<ObjFunction>),
    Native(Arc<ObjNative>),
}

impl Value {
    pub fn as_bool(&self) -> Option<&bool> {
        if let Self::Boolean(bool) = self {
            Some(bool)
        } else {
            None
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Value::Object(_) => {
                unimplemented!("type signatures for objects")
            }
            Value::Long(_) => Type::Long,
            Value::Int(_) => Type::Int,
            Value::Char(_) => Type::Char,
            Value::Boolean(_) => Type::Boolean,
            Value::Byte(_) => Type::Byte,
            Value::String(_) => Type::String,
            Value::Double(_) => Type::Double,
            Value::Float(_) => Type::Float,
            Value::Function(f) => Type::Function(FnSignature {
                input: Box::from(f.params_ty()),
                output: f.return_ty().map(|b| Box::new(b.clone())),
            }),
            Value::Native(_) => Type::NativeFn,
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Long(value)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Self::Int(value)
    }
}

impl From<i8> for Value {
    fn from(value: i8) -> Self {
        Self::Byte(value)
    }
}

impl From<f32> for Value {
    fn from(value: f32) -> Self {
        Self::Float(value)
    }
}
impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Double(value)
    }
}
impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl Neg for Value {
    type Output = Result<Value, VmError>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Long(l) => Ok(Value::Long(-l)),
            Value::Int(i) => Ok(Value::Int(-i)),
            Value::Byte(b) => Ok(Value::Byte(-b)),
            Value::Double(d) => Ok(Value::Double(-d)),
            Value::Float(f) => Ok(Value::Float(-f)),
            Value::Boolean(f) => Ok(Value::Boolean(!f)),
            _ => Err(VmError::IllegalOperation("Negate".to_string(), self)),
        }
    }
}

impl Add for Value {
    type Output = Result<Value, VmError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Value::Long(l), Value::Long(r)) => Ok(Value::Long(l + r)),
            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Byte(l + r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
            (Value::Double(l), Value::Double(r)) => Ok(Value::Double(l + r)),
            (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
            _ => Err(VmError::IllegalBinaryOperation(
                "Add".to_string(),
                self,
                rhs,
            )),
        }
    }
}

impl Sub for Value {
    type Output = Result<Value, VmError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Value::Long(l), Value::Long(r)) => Ok(Value::Long(l - r)),
            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Byte(l - r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
            (Value::Double(l), Value::Double(r)) => Ok(Value::Double(l - r)),
            _ => Err(VmError::IllegalBinaryOperation(
                "Sub".to_string(),
                self,
                rhs,
            )),
        }
    }
}

impl Mul for Value {
    type Output = Result<Value, VmError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Value::Long(l), Value::Long(r)) => Ok(Value::Long(l.saturating_mul(*r))),
            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l.saturating_mul(*r))),
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Byte(l.saturating_mul(*r))),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
            (Value::Double(l), Value::Double(r)) => Ok(Value::Double(l * r)),
            _ => Err(VmError::IllegalBinaryOperation(
                "Mul".to_string(),
                self,
                rhs,
            )),
        }
    }
}

impl Div for Value {
    type Output = Result<Value, VmError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Value::Long(l), Value::Long(r)) => Ok(Value::Long(l.saturating_div(*r))),
            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l.saturating_div(*r))),
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Byte(l.saturating_div(*r))),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l / r)),
            (Value::Double(l), Value::Double(r)) => Ok(Value::Double(l / r)),
            _ => Err(VmError::IllegalBinaryOperation(
                "Div".to_string(),
                self,
                rhs,
            )),
        }
    }
}

impl BitAnd for Value {
    type Output = Result<Value, VmError>;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(*l & *r)),
            _ => Err(VmError::IllegalBinaryOperation(
                "And".to_string(),
                self,
                rhs,
            )),
        }
    }
}

impl BitOr for Value {
    type Output = Result<Value, VmError>;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(*l | *r)),
            _ => Err(VmError::IllegalBinaryOperation(
                "And".to_string(),
                self,
                rhs,
            )),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Object(_) => {
                write!(f, "<object>")
            }
            Value::Long(l) => {
                write!(f, "{l}l")
            }
            Value::Int(l) => {
                write!(f, "{l}i")
            }
            Value::Char(l) => {
                write!(f, "{l}c")
            }
            Value::Boolean(l) => {
                write!(f, "{l}z")
            }
            Value::Byte(l) => {
                write!(f, "{l}b")
            }
            Value::String(l) => {
                write!(f, "{l}")
            }
            Value::Double(d) => {
                write!(f, "{d}d")
            }
            Value::Float(d) => {
                write!(f, "f{d}f")
            }
            Value::Function(func) => {
                write!(f, "func<{}>", func.name())
            }
            Value::Native(native) => {
                write!(f, "native<{}>", native.name())
            }
        }
    }
}
