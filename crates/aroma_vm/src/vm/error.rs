use std::convert::Infallible;
use crate::chunk::{Constant, UnknownOpcode};
use crate::types::Value;

#[derive(Debug, thiserror::Error, Clone)]
pub enum VmError {
    #[error("Main thread ended in exception {0:?}")]
    MainThreadEndedExceptionally(Value),
    #[error("{}", .0.as_ref().map(|p| format!("Thread Panicked: {p}")).unwrap_or_else(|| "Thead Panicked".to_string()))]
    ThreadPanicked(Option<String>),
    #[error(transparent)]
    UnknownOpcode(#[from] UnknownOpcode),
    #[error("No value on stack")]
    NoValueOnStack,
    #[error("Can not {0} on {1}")]
    IllegalOperation(String, Value),
    #[error("Can not {0} on {1} and {2}")]
    IllegalBinaryOperation(String, Value, Value),
    #[error("No instruction")]
    NoInstruction,
    #[error("No constant at index {0}")]
    NoConstant(u8),
    #[error("{0}")]
    Custom(&'static str),
    #[error("Local {0} undefined")]
    NoLocalError(u8),
    #[error("Global {0:?} undefined")]
    NoGlobal(String),
    #[error("const bytecode can not be executed when referring to non-value constant")]
    ConstInstructionWithNonValue(Constant),
    #[error("Boolean expected")]
    BooleanExpected,
    #[error("Function {0:?} not defined")]
    FunctionNotDefined(String),
    #[error("Function {0:?} not loaded")]
    FunctionNotLoaded(String),
    #[error("Function {0:?} previously defined")]
    FunctionAlreadyDefined(String),
    #[error("Constant {0:?} was not expected at this point")]
    UnexpectedConstant(Constant),
    #[error("Expected type {1} but got {0:?}")]
    TypeError(Value, String),
}

impl From<&'static str> for VmError {
    fn from(value: &'static str) -> Self {
        Self::Custom(value)
    }
}

impl From<Infallible> for VmError {
    fn from(value: Infallible) -> Self {
        panic!("infallible can never be created")
    }
}