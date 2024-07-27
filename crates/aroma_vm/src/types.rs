use std::fmt::{Display, Formatter};
use aroma_gc::Trace;
pub use obj::Obj;

use crate::vm::ObjectPtr;

mod obj;

/// A function ptr
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct FunctionPtr(usize);

/// A value that can be stored in a stack frame
#[derive(Debug, Clone, Trace)]
pub enum Value {
    Object(ObjectPtr),
    Long(i64),
    Int(i32),
    Char(char),
    Boolean(bool),
    Byte(i8),
    String(#[req_static] String),
    Double(f64),
    Float(f64),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Object(_) => {
                write!(f, "<object>")
            }
            Value::Long(l) => {
                write!(f, "{l}")
            }
            Value::Int(l) => {
                write!(f, "{l}")
            }
            Value::Char(l) => {
                write!(f, "{l}")
            }
            Value::Boolean(l) => {
                write!(f, "{l}")
            }
            Value::Byte(l) => {
                write!(f, "{l}")
            }
            Value::String(l) => {
                write!(f, "{l}")
            }
            Value::Double(d) => {
                write!(f, "{d}")
            }
            Value::Float(d) => {
                write!(f, "{d}")
            }
        }
    }
}
