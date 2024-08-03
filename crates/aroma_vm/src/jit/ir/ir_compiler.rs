//! Responsible for compiling IR from bytecode

use std::collections::BTreeSet;

use crate::types::function::ObjFunction;
use crate::vm::InsPtr;

#[derive(Debug)]
pub struct IrCompiler {}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
struct Jump {
    from: InsPtr,
    to: InsPtr,
}

fn get_jumps(func: &ObjFunction) -> CompileIrResult<BTreeSet<Jump>> {
    let mut ip: InsPtr = (0, 0);
    let mut jumps = BTreeSet::new();

    let get_to_ip = |start: InsPtr, offset: isize| -> CompileIrResult<InsPtr> {
        get_ip_from_start_with_linear_offset(func, start, offset)
    };

    while let Some(chunk) = func.chunks().get(ip.0) {
        while chunk.len() < ip.1 {

        }
    }

    Ok(jumps)
}

fn get_ip_from_start_with_linear_offset(
    func: &ObjFunction,
    start: InsPtr,
    offset: isize,
) -> Result<InsPtr, CompileIrError> {
    let mut remaining = offset;
    let mut current = start;
    let mut chunk_ptr = func
        .chunks()
        .get(start.0)
        .ok_or(CompileIrError::InstructionPointerOutOfBounds(start))?;
    if remaining > 0 {
        let mut remaining = remaining as usize;
        while remaining > 0 {
            let take = chunk_ptr.len().min(remaining);
            remaining -= take;
            if remaining > 0 {
                current.1 = 0;
                current.0 += 1;
                chunk_ptr = func
                    .chunks()
                    .get(start.0)
                    .ok_or(CompileIrError::InstructionPointerOutOfBounds(start))?;
            } else {
                current.1 += take;
            }
        }
    } else if remaining < 0 {
        let mut remaining = remaining as usize;
        while remaining > 0 {
            let take = current.1.min(remaining);
            remaining -= take;
            if remaining > 0 {
                current.1 = 0;
            } else {
                current.1 += take;
            }
            if let Some(prev) = current.0.checked_sub(1) {
                current.0 = prev;
                chunk_ptr = func
                    .chunks()
                    .get(start.0)
                    .ok_or(CompileIrError::InstructionPointerOutOfBounds(start))?;
                current.1 = chunk_ptr.len() - 1;
            } else {
                return Err(CompileIrError::ChunkIndexOutOfBounds(-1));
            }
        }
    }
    Ok(current)
}

/// An IR compilation error
#[derive(Debug, thiserror::Error)]
pub enum CompileIrError {
    #[error("{}:{} is out of bounds", .0.0, .0.1)]
    InstructionPointerOutOfBounds(InsPtr),
    #[error("Chunk index out of bounds: {0}")]
    ChunkIndexOutOfBounds(isize),
}

pub type CompileIrResult<T> = Result<T, CompileIrError>;
