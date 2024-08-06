use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::{Debug, Formatter};

use indexmap::IndexMap;
use itertools::Itertools;
use log::trace;
use crate::chunk::OpCode;
use crate::jit::ir::CompileIrError;
use crate::jit::ir::ir_compiler::CompileIrResult;
use crate::jit::ir::ir_op::{IrOp, IrValue, IrValueFactory, IrVariable};
use crate::types::{Type, Value};
use crate::types::function::FnSignature;
use crate::vm::StaticFunctionTable;

/// A block is a specialized version of chunks where there's only one entry point and exit point
pub struct IrBlock {
    id: Block,
    parameters: Vec<IrValue>,
    ops: IndexMap<IrValue, IrOp>,
    value_factory: IrValueFactory,
    packed: bool,
}

impl IrBlock {
    /// Creates a new block from an existing value factory
    fn new(block: Block, value_factory: &IrValueFactory) -> Self {
        Self {
            id: block,
            parameters: vec![],
            ops: IndexMap::new(),
            value_factory: value_factory.clone(),
            packed: false,
        }
    }

    fn add_parameter(&mut self, ty: Type) -> IrValue {
        let v = self.value_factory.next(ty);
        self.parameters.push(v.clone());
        v
    }

    /// Gets the blocks followed by this block.
    pub fn followed_by(&self) -> Option<HashSet<Block>> {
        match self.ops.last().map(|v| v.1) {
            Some(IrOp::If {
                then_block: then,
                else_block: otherwise,
                ..
            }) => Some(HashSet::from_iter([*then, *otherwise])),
            Some(IrOp::Jump { block, .. }) => Some(HashSet::from_iter([*block])),
            _ => None,
        }
    }

    pub fn parameters(&self) -> &[IrValue] {
        &self.parameters
    }

    pub fn ops(&self) -> &IndexMap<IrValue, IrOp> {
        &self.ops
    }

    pub fn id(&self) -> Block {
        self.id
    }
}

impl Debug for IrBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.id)?;
        if !self.parameters.is_empty() {
            write!(f, "[")?;
            let joined = self
                .parameters
                .iter()
                .map(|s| format!("{:?}", s))
                .join(", ");
            write!(f, "{}", joined)?;
            write!(f, "]")?;
        }
        writeln!(f, ":")?;
        let ops = self.ops.len();
        for (val, op) in &self.ops[..ops - 1] {
            writeln!(f, "\t{:?} = {}", val, op)?;
        }
        if let Some((_, op)) = self.ops.last() {
            writeln!(f, "\t{}", op)?;
        }

        Ok(())
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
pub struct Block(usize);

impl Debug for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "block {}", self.0)
    }
}

/// An ir function containing blocks
#[derive(Debug)]
pub struct IrFunction {
    entry: Block,
    blocks: IndexMap<Block, IrBlock>,
}

impl IrFunction {
    pub(crate) fn new(entry: Block, blocks: IndexMap<Block, IrBlock>) -> Self {
        Self { entry, blocks }
    }

    pub fn entry(&self) -> Block {
        self.entry
    }

    pub fn blocks(&self) -> &IndexMap<Block, IrBlock> {
        &self.blocks
    }
}

#[derive(Debug)]
pub struct IrBuilder {
    next_block_id: usize,
    current_block: Option<Block>,
    value_factory: IrValueFactory,
    pub(crate) variables: HashMap<IrVariable, (Type, Option<IrValue>)>,
    blocks: BTreeMap<Block, IrBlock>,
    function_defs: StaticFunctionTable,
}

impl IrBuilder {
    /// Creates a new builder
    pub fn new(function_defs: &StaticFunctionTable) -> Self {
        Self {
            next_block_id: 0,
            current_block: None,
            value_factory: IrValueFactory::default(),
            variables: Default::default(),
            blocks: Default::default(),
            function_defs: function_defs.clone(),
        }
    }

    /// Creates a new block
    pub fn create_block(&mut self) -> Block {
        let id = self.next_block_id;
        self.next_block_id += 1;
        let block = Block(id);
        self.blocks
            .insert(block, IrBlock::new(block, &self.value_factory));
        block
    }

    /// Change the block currently being implemented
    pub fn switch_to_block(&mut self, block: Block) {
        self.current_block = Some(block);
    }

    pub fn is_packed(&self, block: Block) -> bool {
        self.blocks[&block].packed
    }

    /// Gets the ops builder for adding operators to the current block
    pub fn ops(&mut self) -> IrOpBuilder {
        let variables = self.variables.clone();
        let functions = self.function_defs.clone();
        let b = self.current_block();

        IrOpBuilder {
            block: b,
            function_defs: functions,
            variables,
        }
    }

    fn current_block(&mut self) -> &mut IrBlock {
        let c = self.current_block.expect("no current block");
        let b = self.blocks.get_mut(&c).expect("no block with given label");
        b
    }

    pub fn add_parameter(&mut self, ty: Type) -> IrValue {
        self.current_block().add_parameter(ty)
    }

    pub fn parameters(&mut self) -> &[IrValue] {
        &self.current_block().parameters
    }

    pub fn declare_var(&mut self, variable: IrVariable, ty: Type) {
        self.variables.insert(variable, (ty, None));
    }

    /// Finishes the current function, creating an ir function
    pub fn finish(self) -> CompileIrResult<IrFunction> {
        trace!("creating function from blocks: {:#?}", self.blocks);
        let mut entry_contenders = HashSet::from_iter(self.blocks.keys().copied());
        let mut packed_blocks = IndexMap::with_capacity(self.blocks.len());
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
            1 => Ok(IrFunction::new(
                *entry_contenders.iter().next().unwrap(),
                packed_blocks,
            )),
            more => {
                panic!("too many potential entry blocks: {more}")
            }
        }
    }
}

/// Responsible for adding instructions to the current block
pub struct IrOpBuilder<'a> {
    block: &'a mut IrBlock,
    function_defs: StaticFunctionTable,
    variables: HashMap<IrVariable, (Type, Option<IrValue>)>,
}

impl<'a> IrOpBuilder<'a> {
    pub fn ret(&mut self, value: Option<IrValue>) {
        self.assert_not_packed();
        self.block.ops.insert(
            self.block.value_factory.next(Type::Never),
            IrOp::Return(value),
        );
        self.block.packed = true;
    }

    pub fn branch_if(
        &mut self,
        cond: IrValue,
        then_block: Block,
        then_params: &[IrValue],
        else_block: Block,
        else_params: &[IrValue],
    ) {
        self.assert_not_packed();
        self.block.ops.insert(
            self.block.value_factory.next(Type::Never),
            IrOp::If {
                cond,
                then_block,
                then_params: Vec::from(then_params),
                else_block,
                else_params: Vec::from(else_params),
            },
        );
        self.block.packed = true;
    }

    pub fn jump(&mut self, next: Block, next_params: &[IrValue]) {
        self.assert_not_packed();
        self.block.ops.insert(
            self.block.value_factory.next(Type::Never),
            IrOp::Jump {
                block: next,
                block_params: Vec::from(next_params),
            },
        );
        self.block.packed = true;
    }

    pub fn iconst(&mut self, v: Value) -> IrValue {
        self.assert_not_packed();
        let value = self.block.value_factory.next(v.get_type());
        self.block.ops.insert(value.clone(), IrOp::Constant(v));
        value
    }

    pub fn function_ref(&mut self, id: &str) -> CompileIrResult<IrValue> {
        self.assert_not_packed();
        let function_defs = self.function_defs.read();
        let func = function_defs
            .get(id)
            .ok_or_else(|| CompileIrError::FunctionNotDefined(id.to_string()))?;
        let value = self
            .block
            .value_factory
            .next(Type::Function(func.signature()));
        self.block
            .ops
            .insert(value.clone(), IrOp::Function(id.to_string()));
        Ok(value)
    }

    pub fn binary_op(&mut self, op_code: OpCode, a: IrValue, b: IrValue) -> IrValue {
        self.assert_not_packed();

        let ty = match op_code {
            OpCode::Eq
            | OpCode::Neq
            | OpCode::Gt
            | OpCode::Gte
            | OpCode::Lt
            | OpCode::Lte
            | OpCode::And
            | OpCode::Not
            | OpCode::Or => Type::Boolean,
            _ => a.get_type().clone(),
        };

        let value = self.block.value_factory.next(ty);
        self.block
            .ops
            .insert(value.clone(), IrOp::BinOp(op_code, a, b));
        value
    }

    pub fn unary_op(&mut self, op_code: OpCode, a: IrValue) -> IrValue {
        self.assert_not_packed();
        let value = self.block.value_factory.next(a.get_type().clone());
        self.block
            .ops
            .insert(value.clone(), IrOp::UniOp(op_code, a));
        value
    }

    pub fn set_local_var(&mut self, ir_variable: IrVariable, value: IrValue) {
        self.assert_not_packed();
        self.block.ops.insert(
            self.block.value_factory.next(Type::Never),
            IrOp::Assign(ir_variable.0, value),
        );
    }

    pub fn get_local_var(&mut self, ir_variable: IrVariable) -> IrValue {
        self.assert_not_packed();
        let ret = self
            .block
            .value_factory
            .next(self.variables.get(&ir_variable).unwrap().0.clone());
        self.block
            .ops
            .insert(ret.clone(), IrOp::GetLocal(ir_variable.0));
        ret
    }

    pub fn call(
        &mut self,
        callee: IrValue,
        params: &[IrValue],
        fn_signature: &FnSignature,
    ) -> Option<IrValue> {
        if let Some(ret_type) = fn_signature.ret() {
            let ret = self.block.value_factory.next(ret_type.clone());
            self.block
                .ops
                .insert(ret.clone(), IrOp::Call(callee, Vec::from(params)));

            Some(ret)
        } else {
            todo!("void function")
        }
    }

    #[inline]
    fn assert_not_packed(&self) {
        assert!(
            !self.block.packed,
            "Can not add to block that has been packed"
        );
    }
}
