use std::cell::Cell;
use std::fmt::{Debug, Display, Formatter};
use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use derive_more::From;

use crate::chunk::OpCode;
use crate::jit::ir::ir_builder::Block;
use crate::types::{Type, Value};

/// An intermediate value
#[derive(Clone, Hash, Eq, PartialEq, From)]
pub struct IrValue(Type, usize);

impl IrValue {
    pub fn get_type(&self) -> &Type {
        &self.0
    }
}

impl Debug for IrValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}: {}", self.1, self.0)
    }
}

/// An intermediate value
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct IrVariable(pub usize, PhantomData<()>);

impl From<usize> for IrVariable {
    fn from(value: usize) -> Self {
        Self(value, PhantomData)
    }
}

impl From<u8> for IrVariable {
    fn from(value: u8) -> Self {
        Self::from(value as usize)
    }
}

pub type IrVariableFactory = IdFactory<IrVariable>;

#[derive(Debug, Default, Clone)]
pub struct IrValueFactory(IdFactory<usize>);

impl IrValueFactory {

    pub fn next(&mut self, ty: Type) -> IrValue {
        IrValue(ty, self.0.next())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrOp {
    CallFunction(String, Vec<IrValue>),
    Param(usize),
    Constant(Value),
    Function(String),
    Return(Option<IrValue>),
    Assign(usize, IrValue),
    BinOp(OpCode, IrValue, IrValue),
    UniOp(OpCode, IrValue),
    Call(IrValue, Vec<IrValue>),
    GetLocal(usize),
    If {
        cond: IrValue,
        then_block: Block,
        then_params: Vec<IrValue>,
        else_block: Block,
        else_params: Vec<IrValue>,
    },
    Jump {
        block: Block,
        block_params: Vec<IrValue>,
    },
}

impl Display for IrOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IrOp::Constant(c) => {
                write!(f, "iconst {c}")
            }
            IrOp::Return(r) => match r {
                None => {
                    write!(f, "ret")
                }
                Some(v) => {
                    write!(f, "ret {v:?}")
                }
            },
            IrOp::Assign(var, v) => {
                write!(f, "set_var {var}, {v:?}")
            }
            IrOp::BinOp(op, a, b) => {
                write!(f, "{op} {a:?}, {b:?}")
            }
            IrOp::UniOp(op, v) => {
                write!(f, "{op} {v:?}")
            }
            IrOp::GetLocal(v) => {
                write!(f, "get_var {v}")
            }
            IrOp::If {
                cond,
                then_block,
                then_params,
                else_block,
                else_params,
            } => {
                write!(
                    f,
                    "brif {cond:?}, {then_block:?}{then_params:?}, {else_block:?}{else_params:?}"
                )
            }
            IrOp::Jump {
                block,
                block_params,
            } => {
                write!(f, "jmp {block:?}{block_params:?}",)
            }
            IrOp::Function(func) => {
                write!(f, "get_function({func:?})")
            }
            IrOp::Call(callee, params) => {
                write!(f, "call {callee:?}, {params:?}")
            }
            _other => todo!("{_other:?}"),
        }
    }
}

/// Creates values
#[derive(Debug, Clone)]
pub struct IdFactory<T>(Arc<AtomicUsize>, PhantomData<T>);

impl<T: From<usize>> IdFactory<T> {
    pub fn new() -> Self {
        Self(Default::default(), PhantomData)
    }

    pub fn next(&mut self) -> T {
        let v = self.0.fetch_add(1, Ordering::SeqCst);
        T::from(v)
    }
}

impl<T: From<usize>> Default for IdFactory<T> {
    fn default() -> Self {
        Self::new()
    }
}
