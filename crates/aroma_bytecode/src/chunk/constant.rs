use derive_more::TryInto;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, TryInto)]
pub enum Constant {
    Int(i32),
    Long(i64),
    String(u8),
    FunctionId(u8),
    Utf8(&'static str),
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Int(i) => {
                write!(f, "int({i})")
            }
            Constant::Long(l) => {
                write!(f, "long({l})")
            }
            Constant::String(s) => {
                write!(f, "string(#{s})")
            }
            Constant::FunctionId(fid) => {
                write!(f, "function_ref(#{fid})")
            }
            Constant::Utf8(utf8) => {
                write!(f, "{utf8:?}")
            }
        }
    }
}
