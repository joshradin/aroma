use std::sync::Arc;
use crate::chunk::{Chunk, OpCode};
use crate::types::function::ObjFunction;
use crate::types::Value;

#[derive(Debug)]
pub enum Op {
    Constant(Value),
    Return(Box<Op>),
    Assign(usize, Box<Op>)
}

/// Converts some byte code to some ir
pub fn to_ir(f: &ObjFunction) -> Vec<Op> {
    let mut ops = vec![];


}

fn chunk_to_ir(chunk: &Chunk, ops: &mut Vec<Op>) {

}


fn get_instruction_offsets(chunk: &Chunk) -> Vec<usize> {
    let mut accum = Vec::with_capacity(chunk.len());
    let mut offset = 0;
    while offset < chunk.len() {
        accum.push(offset);
        let opcode = OpCode::try_from(chunk.code()[offset]).unwrap();
        let offset = match opcode {
            OpCode::Constant | OpCode::Call => offset + 2,
            OpCode::Jump | OpCode::JumpIfFalse => offset + 3,
            _ => offset + 1
        };
    }
    accum.shrink_to_fit();
    accum
}