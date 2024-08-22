//! Responsible for compiling IR from bytecode

use itertools::Itertools;
use log::trace;
use petgraph::data::Build;
use petgraph::graph::NodeIndex;
use rangemap::RangeMap;
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::marker::PhantomData;
use std::sync::Arc;

use crate::jit::ir::ir_builder::{Block, IrBuilder, IrFunction};
use crate::jit::ir::ir_compiler::offset_graph::{get_offset_graph, OffsetGraph};
use crate::jit::ir::ir_op::{IrValue, IrVariable, IrVariableFactory};
use crate::types::function::ObjFunction;
use crate::types::{Type, Value};
use crate::vm::{InsPtr, StaticFunctionTable};
use aroma_bytecode::chunk::Constant;
use aroma_bytecode::chunk::{
    Chunk, ChunkVisitor, ContinuousOffsetVisitor, IntoOpcodeIterator, OpCode,
};

mod offset_graph;

/// Responsible with compiling bytecode into [IrFunction]s
#[derive(Debug)]
pub struct IrCompiler {
    _d: PhantomData<()>,
    linearized: Vec<u8>,
    rev_mapping: RangeMap<usize, usize>,
    node_index_to_block: HashMap<NodeIndex, Block>,
    offset_graph: OffsetGraph,
    chunks: BTreeMap<usize, Arc<Chunk>>,
    variable_factory: IrVariableFactory,
    function_defs: StaticFunctionTable,
}

impl IrCompiler {
    /// Creates a new compiler
    pub fn new(function_defs: &StaticFunctionTable) -> Self {
        Self {
            _d: PhantomData,
            linearized: Vec::new(),
            rev_mapping: RangeMap::new(),
            node_index_to_block: Default::default(),
            offset_graph: OffsetGraph::default(),
            chunks: BTreeMap::new(),
            variable_factory: IrVariableFactory::new(),
            function_defs: function_defs.clone(),
        }
    }

    /// Compile a function from aroma bytecode to a function of [IrOp](super::ir_builder::IrOp) and blocks
    pub fn compile(&mut self, func: &ObjFunction) -> CompileIrResult<IrFunction> {
        (self.linearized, self.rev_mapping) = linearize(func.chunks());
        self.chunks = BTreeMap::from_iter(
            func.chunks()
                .iter()
                .enumerate()
                .map(|(i, c)| (i, c.clone())),
        );

        self.offset_graph = get_offset_graph(&self.linearized);
        trace!("offset_graph: {:#?}", self.offset_graph);

        let mut builder = IrBuilder::new(&self.function_defs);

        for (idx, ty) in func.params_ty().iter().enumerate() {
            builder.declare_var(IrVariable::from(idx), ty.clone());
        }
        for (idx, ty) in func.variables().iter().enumerate() {
            builder.declare_var(IrVariable::from(idx + func.arity()), ty.clone());
        }

        self.node_index_to_block = HashMap::<NodeIndex, Block>::new();
        self.offset_graph.node_indices().for_each(|idx| {
            self.node_index_to_block.insert(idx, builder.create_block());
        });
        let mut node_idx_q = VecDeque::new();
        let entry_idx = self
            .offset_graph
            .node_indices()
            .find(|idx| self.offset_graph[*idx].from == 0)
            .unwrap();
        node_idx_q.push_front(entry_idx);
        let mut node_idx_visited = HashSet::new();
        let mut block_to_starting = HashMap::<Block, Vec<IrValue>>::new();

        let entry_block = self.node_index_to_block[&entry_idx];
        builder.switch_to_block(entry_block);

        for x in func.params_ty() {
            builder.add_parameter(x.clone());
        }

        loop {
            trace!("queue: {node_idx_q:?}");
            let Some(node_idx) = node_idx_q.pop_front() else {
                break;
            };
            if node_idx_visited.contains(&node_idx) {
                continue;
            } else {
                node_idx_visited.insert(node_idx);
            }

            let block = *self.node_index_to_block.get(&node_idx).unwrap();
            builder.switch_to_block(block);
            if let Some(starting) = block_to_starting.get(&block) {
                for ir_value in starting.iter() {
                    builder.add_parameter(ir_value.get_type().clone());
                }
            }

            let remaining = self.compile_node(&mut builder, node_idx, block)?;
            trace!("remaining for {block:?} = {:?}", remaining);

            let adj = self.offset_graph.neighbors(node_idx).collect::<Vec<_>>();
            for adjacent in &adj {
                node_idx_q.push_back(*adjacent);
                let adj_blk = self.node_index_to_block[adjacent];
                trace!("found block adj {:?} -> {:?}", block, adj_blk);
                block_to_starting.insert(adj_blk, remaining.clone());
            }
            if !builder.is_packed(block) && adj.len() == 1 {
                let next = self.node_index_to_block[&adj[0]];
                builder.ops().jump(next, &remaining);
            }
        }

        builder.finish()
    }

    /// visits chunks in order
    fn visit<V: ChunkVisitor>(&self, visitor: V) -> Result<(), V::Err> {
        let mut linear_visitor = ContinuousOffsetVisitor::new(visitor);
        for chunk in self.chunks.values() {
            chunk.visit(&mut linear_visitor)?;
        }
        Ok(())
    }

    fn get_chunk_for_byte_offset(&self, offset: usize) -> CompileIrResult<&Chunk> {
        let index = *self
            .rev_mapping
            .get(&offset)
            .ok_or(CompileIrError::LinearizedOffsetOutOfBounds(offset))?;
        self.chunks
            .get(&index)
            .map(|c| c.as_ref())
            .ok_or(CompileIrError::ChunkIndexOutOfBounds(index as isize))
    }

    fn compile_node(
        &mut self,
        builder: &mut IrBuilder,
        node: NodeIndex,
        block: Block,
    ) -> CompileIrResult<Vec<IrValue>> {
        let offset_range = self.offset_graph[node];
        let bytecode = &self.linearized[offset_range.from..offset_range.to];
        trace!("bytecode for block {block:?}: {:x?}", bytecode);
        let mut value_stack: Vec<IrValue> = vec![];

        for x in builder.parameters().iter() {
            value_stack.push(x.clone());
        }

        for (offset, opcode, bytes) in bytecode.into_opcode_iter() {
            trace!("compiling {opcode}{bytes:x?}");
            trace!("current value stack: {:?}", value_stack);
            let chunk = self.get_chunk_for_byte_offset(offset_range.from + offset)?;
            match opcode {
                OpCode::Constant => {
                    let idx = u8::from_be_bytes(bytes.try_into().unwrap());
                    let constant = chunk.get_constant(idx).unwrap();
                    let value = match *constant {
                        Constant::Int(i) => builder.ops().iconst(Value::from(i)),
                        Constant::Long(l) => builder.ops().iconst(Value::from(l)),
                        Constant::String(utf8_idx) => {
                            let Some(Constant::Utf8(s)) = chunk.get_constant(utf8_idx) else {
                                panic!("utf8_idx must always point to a utf8 constant pool entry")
                            };
                            builder.ops().iconst(Value::from(s.to_string()))
                        }
                        Constant::FunctionId(utf8_idx) => {
                            let Some(Constant::Utf8(s)) = chunk.get_constant(utf8_idx) else {
                                panic!("utf8_idx must always point to a utf8 constant pool entry")
                            };
                            builder.ops().function_ref(s)?
                        }
                        _other => panic!("Can't handle constant {_other}"),
                    };
                    value_stack.push(value);
                }
                OpCode::Return => {
                    builder.ops().ret(value_stack.pop());
                }
                OpCode::Negate => {
                    let a = value_stack
                        .pop()
                        .ok_or_else(|| CompileIrError::NoValueAvailable)?;
                    let v = builder.ops().unary_op(opcode, a);
                    value_stack.push(v);
                }
                OpCode::Add
                | OpCode::Subtract
                | OpCode::Mult
                | OpCode::Divide
                | OpCode::Eq
                | OpCode::Neq
                | OpCode::Gt
                | OpCode::Gte
                | OpCode::Lt
                | OpCode::Lte
                | OpCode::And
                | OpCode::Not
                | OpCode::Or => {
                    let b = value_stack
                        .pop()
                        .ok_or_else(|| CompileIrError::NoValueAvailable)?;
                    let a = value_stack
                        .pop()
                        .ok_or_else(|| CompileIrError::NoValueAvailable)?;
                    let v = builder.ops().binary_op(opcode, a, b);
                    value_stack.push(v);
                }
                OpCode::Pop => {
                    value_stack
                        .pop()
                        .ok_or_else(|| CompileIrError::NoValueAvailable)?;
                }
                OpCode::LtoI => {}
                OpCode::IToL => {}
                OpCode::SetLocalVar => {
                    let var_idx = u8::from_be_bytes(bytes.try_into().unwrap());
                    let a = value_stack
                        .pop()
                        .ok_or_else(|| CompileIrError::NoValueAvailable)?;

                    let var = IrVariable::from(var_idx);

                    builder.ops().set_local_var(var, a);
                }
                OpCode::GetLocalVar => {
                    let var_idx = u8::from_be_bytes(bytes.try_into().unwrap());
                    let var = IrVariable::from(var_idx);
                    let v = builder.ops().get_local_var(var);
                    value_stack.push(v);
                }
                OpCode::GetGlobalVar => {}
                OpCode::SetGlobalVar => {}
                OpCode::JumpIfFalse => {
                    let cond = value_stack
                        .last()
                        .ok_or_else(|| CompileIrError::NoValueAvailable)?;
                    let jump_offset = u16::from_be_bytes(bytes.try_into().unwrap());
                    let to_offset = offset_range.from + offset + 3 + jump_offset as usize;
                    let fall_through_offset = offset_range.from + offset + 3;
                    let else_block = self
                        .offset_graph
                        .node_indices()
                        .find_map(|node_idx| {
                            let s = self.offset_graph[node_idx];
                            if s.from == to_offset {
                                Some(self.node_index_to_block[&node_idx])
                            } else {
                                None
                            }
                        })
                        .ok_or_else(|| CompileIrError::NoBlockStartsAtOffset(to_offset))?;
                    let then_block = self
                        .offset_graph
                        .node_indices()
                        .find_map(|node_idx| {
                            let s = self.offset_graph[node_idx];
                            if s.from == fall_through_offset {
                                Some(self.node_index_to_block[&node_idx])
                            } else {
                                None
                            }
                        })
                        .ok_or_else(|| CompileIrError::NoBlockStartsAtOffset(to_offset))?;
                    builder.ops().branch_if(
                        cond.clone(),
                        then_block,
                        &value_stack,
                        else_block,
                        &value_stack,
                    );
                }
                OpCode::Jump => {
                    let jump_offset = u16::from_be_bytes(bytes.try_into().unwrap());
                    let to_offset = offset_range.from + offset + 3 + jump_offset as usize;
                    let block = self
                        .offset_graph
                        .node_indices()
                        .find_map(|node_idx| {
                            let s = self.offset_graph[node_idx];
                            if s.from == to_offset {
                                Some(self.node_index_to_block[&node_idx])
                            } else {
                                None
                            }
                        })
                        .ok_or_else(|| CompileIrError::NoBlockStartsAtOffset(to_offset))?;
                    builder.ops().jump(block, &value_stack);
                }
                OpCode::Loop => {
                    let jump_offset = -i32::from(u16::from_be_bytes(bytes.try_into().unwrap()));
                    let to_offset = (offset_range.from + offset + 3)
                        .saturating_add_signed(jump_offset as isize);
                    let block = self
                        .offset_graph
                        .node_indices()
                        .find_map(|node_idx| {
                            let s = self.offset_graph[node_idx];
                            if s.from == to_offset {
                                Some(self.node_index_to_block[&node_idx])
                            } else {
                                None
                            }
                        })
                        .ok_or_else(|| CompileIrError::NoBlockStartsAtOffset(to_offset))?;
                    builder.ops().jump(block, &value_stack);
                }
                OpCode::Call => {
                    let callee = value_stack
                        .pop()
                        .ok_or_else(|| CompileIrError::NoValueAvailable)?;
                    let arg_c = u8::from_be_bytes(bytes.try_into().unwrap());
                    let mut values = vec![];
                    for _ in 0..arg_c {
                        values.push(
                            value_stack
                                .pop()
                                .ok_or_else(|| CompileIrError::NoValueAvailable)?,
                        );
                    }
                    let signature = callee
                        .get_type()
                        .as_fn_signature()
                        .ok_or_else(|| {
                            CompileIrError::CalleeNotCallable(callee.get_type().clone())
                        })?
                        .clone();
                    let output = builder.ops().call(callee, &values, &signature);
                    if let Some(output) = output {
                        value_stack.push(output);
                    }
                }
                _ => {}
            }
        }

        Ok(value_stack)
    }
}

fn linearize(chunks: &[Arc<Chunk>]) -> (Vec<u8>, RangeMap<usize, usize>) {
    let mut bytes = vec![];
    let mut range_map = RangeMap::new();
    let offset = 0;
    for (idx, chunk) in chunks.iter().enumerate() {
        let len = chunk.code().len();
        range_map.insert(offset..(offset + len), idx);
        bytes.extend(chunk.code())
    }

    (bytes, range_map)
}

/// An IR compilation error
#[derive(Debug, thiserror::Error)]
pub enum CompileIrError {
    #[error("{}:{} is out of bounds", .0.0, .0.1)]
    InstructionPointerOutOfBounds(InsPtr),
    #[error("Chunk index out of bounds: {0}")]
    ChunkIndexOutOfBounds(isize),
    #[error("{} is out of bounds", .0)]
    LinearizedOffsetOutOfBounds(usize),
    #[error("variable {0:} is not declared")]
    VariableUndeclared(u8),
    #[error("variable {0:} is declared but has no value")]
    VariableUndefined(u8),
    #[error("No block starts at offset {0}")]
    NoBlockStartsAtOffset(usize),
    #[error("No value available")]
    NoValueAvailable,
    #[error("{0} is not a callable value")]
    CalleeNotCallable(Type),
    #[error("{0:?} is not defined")]
    FunctionNotDefined(String),
}

pub type CompileIrResult<T> = Result<T, CompileIrError>;

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use test_log::test;

    use crate::debug::Disassembler;
    use crate::examples::{factorial, fibonacci};
    use crate::jit::ir::ir_compiler::IrCompiler;
    use crate::types::function::ObjFunction;
    use crate::vm::StaticFunctionTable;

    fn can_compile(func: ObjFunction) {
        let func = Arc::new(func);
        let static_functions = StaticFunctionTable::default();
        static_functions
            .write()
            .insert(func.name().to_owned(), func.clone());
        let mut compiler = IrCompiler::new(&static_functions);
        Disassembler.disassemble_function(&func).unwrap();
        let compiled = compiler.compile(&func);

        assert!(
            matches!(compiled, Ok(_)),
            "Could not compile function: {}",
            compiled.unwrap_err()
        );
    }

    #[test]
    fn test_compile_recursive() {
        can_compile(fibonacci());
    }
    #[test]
    fn test_compile_looping() {
        can_compile(factorial());
    }
}
