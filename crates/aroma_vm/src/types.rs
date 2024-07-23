use std::fmt::{Display, Formatter};

use gc_arena::Collect;

pub use obj::Obj;

use crate::vm::ObjectPtr;

mod obj;

/// A function ptr
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct FunctionPtr(usize);

/// A value that can be stored in a stack frame
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum Value<'vm> {
    Object(ObjectPtr<'vm>),
    Long(i64),
    Int(i32),
    Char(char),
    Boolean(bool),
    Byte(i8),
    String(String),
    Double(f64),
    Float(f64),
}

impl Display for Value<'static> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Object(_) => {
                write!(f, "<object>")
            }
            Value::Long(l) => {
                write!(f, "{l}")
            }
            Value::Int(l) => { write!(f, "{l}") }
            Value::Char(l) => { write!(f, "{l}") }
            Value::Boolean(l) => { write!(f, "{l}") }
            Value::Byte(l) => { write!(f, "{l}") }
            Value::String(l) => { write!(f, "{l}") }
            Value::Double(d) => { write!(f, "{d}")}
            Value::Float(d) => { write!(f, "{d}")}
        }
    }
}