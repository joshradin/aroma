use std::collections::{HashMap, HashSet};

use crate::chunk::OpCode;
use crate::jit::JitResult;
use crate::types::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum JitIrOp {
    CallFunction(String, Vec<Box<JitIrOp>>),
    Param(usize),
    Constant(Value),
    Function(String),
    Return(Option<Box<JitIrOp>>),
    Assign(usize, Box<JitIrOp>),
    BinOp(OpCode, Box<JitIrOp>, Box<JitIrOp>),
    UniOp(OpCode, Box<JitIrOp>),
    Call(Box<JitIrOp>, Vec<JitIrOp>),
    GetLocal(usize),
    If {
        cond: Box<JitIrOp>,
        then: Block,
        otherwise: Block,
    },
    Jump {
        block: Block,
    },
}

/// A block is a specialized version of chunks where there's only one entry point and exit point
#[derive(Debug, Default)]
pub struct JitIrOpBlock {
    ops: Vec<JitIrOp>,
    packed: bool,
}

impl JitIrOpBlock {
    /// Gets the blocks followed by this block.
    pub fn followed_by(&self) -> Option<HashSet<Block>> {
        match self.ops.last() {
            Some(JitIrOp::If {
                then, otherwise, ..
            }) => Some(HashSet::from_iter([*then, *otherwise])),
            Some(JitIrOp::Jump { block }) => Some(HashSet::from_iter([*block])),
            _ => None,
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
pub struct Block(usize);

/// An ir function containing blocks
#[derive(Debug)]
pub struct IrFunction {
    entry: Block,
    blocks: HashMap<Block, JitIrOpBlock>,
}

#[derive(Debug)]
pub struct IrBuilder {
    next_block_id: usize,
    current_block: Option<Block>,
    blocks: HashMap<Block, JitIrOpBlock>,
}

impl IrBuilder {
    /// Creates a new builder
    pub fn new() -> Self {
        Self {
            next_block_id: 0,
            current_block: None,
            blocks: Default::default(),
        }
    }

    /// Creates a new block
    pub fn create_block(&mut self) -> Block {
        let id = self.next_block_id;
        self.next_block_id += 1;
        let block = Block(id);
        self.blocks.insert(block, JitIrOpBlock::default());
        block
    }

    /// Change the block currently being implemented
    pub fn switch_to_block(&mut self, block: Block) {
        self.current_block = Some(block);
    }

    /// Gets the ops builder for adding operators to the current block
    pub fn ops(&mut self) -> IrOpBuilder {
        let c = self.current_block.expect("no current block");
        let b = self.blocks.get_mut(&c).expect("no block with given label");
        IrOpBuilder { block: b }
    }

    /// Finishes the current function, creating an ir function
    pub fn finish(self) -> JitResult<IrFunction> {
        let mut entry_contenders = HashSet::from_iter(self.blocks.keys().copied());
        let mut packed_blocks = HashMap::with_capacity(self.blocks.len());
        for (blk_id, blk) in self.blocks {
            if blk.packed {
                if let Some(next) = blk.followed_by() {
                    entry_contenders = entry_contenders.difference(&next).copied().collect();
                }
                packed_blocks.insert(blk_id, blk);
            }
        }
        match entry_contenders.len() {
            0 => {
                panic!("no entry blocks")
            }
            1 => {
                todo!("create function")
            }
            more => {
                panic!("too many potential entry blocks: {more}")
            }
        }
    }
}

/// Responsible for adding instructions to the current block
pub struct IrOpBuilder<'a> {
    block: &'a mut JitIrOpBlock,
}

impl<'a> IrOpBuilder<'a> {
    pub fn ret(&mut self) {
        let last = self.block.ops.pop().map(|b| Box::new(b));
        self.block.ops.push(JitIrOp::Return(last));
        self.block.packed = true;
    }

    pub fn branch_if(&mut self, then_block: Block, else_block: Block) {
        let last = self.block.ops.pop().map(|b| Box::new(b));
        self.block.ops.push(JitIrOp::Return(last));
        self.block.packed = true;
    }

    pub fn jump(&mut self, next: Block) {
        let last = self.block.ops.pop().map(|b| Box::new(b));
        self.block.ops.push(JitIrOp::Return(last));
        self.block.packed = true;
    }

    #[inline]
    fn assert_not_packed(&self) {
        assert!(
            !self.block.packed,
            "Can not add to block that has been packed"
        );
    }
}
