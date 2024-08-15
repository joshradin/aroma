use aroma_visitor_gen::visitor;
use std::cell::Cell;

use crate::chunk::constant::Constant;
use crate::chunk::opcode::OpCode;
use crate::chunk::Chunk;

visitor! {
    pub trait ChunkVisitor {
        visit fn opcode(v, offset: usize, opcode: &OpCode) -> Result<()> {
            Ok(())
        }

        visit fn simple_instruction (v, offset: usize, opcode: &OpCode) -> Result<()> {
            v.visit_opcode(offset, opcode)?;
            Ok(())
        }
        visit fn jump_instruction (v, offset: usize, opcode: &OpCode, jmp_offset: u16) -> Result<()> {
            v.visit_opcode(offset, opcode)?;
            Ok(())
        }
        visit fn loop_instruction (v, offset: usize, opcode: &OpCode, jmp_offset: i32) -> Result<()> {
            v.visit_opcode(offset, opcode)?;
            Ok(())
        }
        visit fn constant_instruction (v, offset: usize, opcode: &OpCode, idx: u8, constant: &Constant) -> Result<()> {
            v.visit_opcode(offset, opcode)?;
            Ok(())
        }
        visit fn global_instruction (v, offset: usize, opcode: &OpCode, global: &str) -> Result<()> {
            v.visit_opcode(offset, opcode)?;
            Ok(())
        }
        visit fn local_var_instruction (v, offset: usize, opcode: &OpCode, var_idx: u8) -> Result<()> {
            v.visit_opcode(offset, opcode)?;
            Ok(())
        }

        visit fn closure_instruction (v, offset: usize, opcode: &OpCode, idx: u8, constant: &Constant) -> Result<()> {
            v.visit_opcode(offset, opcode)?;
            Ok(())
        }
    }
}

/// Responsible for driving a [`ChunkVisitor`](ChunkVisitor)
#[derive(Debug)]
pub struct ChunkVisitorDriver<'a, T: ChunkVisitor> {
    chunk: &'a Chunk,
    visitor: T,
}

impl<'a, T: ChunkVisitor> ChunkVisitorDriver<'a, T> {
    /// Creates a new driver
    pub fn new(chunk: &'a Chunk, visitor: T) -> Self {
        Self { chunk, visitor }
    }

    /// Visits the chunk
    pub fn visit(mut self) -> Result<(), T::Err> {
        let mut offset = 0;
        while offset < self.chunk.len() {
            offset = self.visit_instruction(offset)?;
        }
        Ok(())
    }

    fn visit_instruction(&mut self, offset: usize) -> Result<usize, T::Err> {
        let instruction = OpCode::try_from(self.chunk.code()[offset]).unwrap();
        match instruction {
            OpCode::Return
            | OpCode::Negate
            | OpCode::Add
            | OpCode::Divide
            | OpCode::Mult
            | OpCode::Subtract
            | OpCode::Eq
            | OpCode::Neq
            | OpCode::Gt
            | OpCode::Gte
            | OpCode::Lt
            | OpCode::Lte
            | OpCode::And
            | OpCode::Or
            | OpCode::Pop => {
                self.visitor
                    .visit_simple_instruction(offset, &instruction)?;
                Ok(offset + 1)
            }
            OpCode::Constant | OpCode::Call => {
                let constant_idx = self.chunk.code()[offset + 1];
                let constant = self.chunk.get_constant(constant_idx).unwrap();
                self.visitor.visit_constant_instruction(
                    offset,
                    &instruction,
                    constant_idx,
                    constant,
                )?;
                Ok(offset + 2)
            }
            OpCode::Closure => {
                let constant_idx = self.chunk.code()[offset + 1];
                let constant = self.chunk.get_constant(constant_idx).unwrap();
                self.visitor.visit_closure_instruction(
                    offset,
                    &instruction,
                    constant_idx,
                    constant,
                )?;
                Ok(offset + 2)
            }
            OpCode::GetLocalVar | OpCode::SetLocalVar => {
                let var_idx = self.chunk.code()[offset + 1];
                self.visitor
                    .visit_local_var_instruction(offset, &instruction, var_idx)?;
                Ok(offset + 2)
            }
            OpCode::GetGlobalVar | OpCode::SetGlobalVar => {
                let constant_idx = self.chunk.code()[offset + 1];
                let Constant::Utf8(constant) = self.chunk.get_constant(constant_idx).unwrap()
                else {
                    panic!("constant must be utf8");
                };
                self.visitor
                    .visit_global_instruction(offset, &instruction, constant)?;
                Ok(offset + 2)
            }
            OpCode::Jump | OpCode::JumpIfFalse => {
                let fwd_jump = u16::from_be_bytes(
                    <[u8; 2]>::try_from(&self.chunk.code()[offset + 1..][..2]).unwrap(),
                );
                self.visitor
                    .visit_jump_instruction(offset, &instruction, fwd_jump)?;
                Ok(offset + 3)
            }
            OpCode::Loop => {
                let back_jump = u16::from_be_bytes(
                    <[u8; 2]>::try_from(&self.chunk.code()[offset + 1..][..2]).unwrap(),
                );
                let back_jump = -(back_jump as i32);
                self.visitor
                    .visit_loop_instruction(offset, &instruction, back_jump)?;
                Ok(offset + 3)
            }
            _opcode => {
                unimplemented!("unknown opcode")
            }
        }
    }
}

/// Default chunk driver
#[derive(Debug, Default)]
pub struct DefaultChunkDriver;

impl ChunkVisitor for DefaultChunkDriver {
    type Err = ();
}

/// A visitor that maintains a continuous offset that re-used for every chunk visited
pub struct ContinuousOffsetVisitor<V: ChunkVisitor> {
    offset: Cell<usize>,
    visitor: V,
}

impl<V: ChunkVisitor> ContinuousOffsetVisitor<V> {
    pub fn new(visitor: V) -> Self {
        Self {
            offset: Cell::new(0),
            visitor,
        }
    }
}

impl<V: ChunkVisitor> ChunkVisitor for ContinuousOffsetVisitor<V> {
    type Err = V::Err;

    fn visit_simple_instruction(
        &mut self,
        _offset: usize,
        opcode: &OpCode,
    ) -> Result<(), Self::Err> {
        self.visitor
            .visit_simple_instruction(self.offset.get(), opcode)?;
        self.offset.set(self.offset.get() + 1);
        Ok(())
    }

    fn visit_jump_instruction(
        &mut self,
        _offset: usize,
        opcode: &OpCode,
        jmp_offset: u16,
    ) -> Result<(), Self::Err> {
        self.visitor
            .visit_jump_instruction(self.offset.get(), opcode, jmp_offset)?;
        self.offset.set(self.offset.get() + 3);
        Ok(())
    }

    fn visit_loop_instruction(
        &mut self,
        _offset: usize,
        opcode: &OpCode,
        jmp_offset: i32,
    ) -> Result<(), Self::Err> {
        self.visitor
            .visit_loop_instruction(self.offset.get(), opcode, jmp_offset)?;
        self.offset.set(self.offset.get() + 3);
        Ok(())
    }

    fn visit_constant_instruction(
        &mut self,
        _offset: usize,
        opcode: &OpCode,
        idx: u8,
        constant: &Constant,
    ) -> Result<(), Self::Err> {
        self.visitor
            .visit_constant_instruction(self.offset.get(), opcode, idx, constant)?;
        self.offset.set(self.offset.get() + 2);
        Ok(())
    }

    fn visit_global_instruction(
        &mut self,
        _offset: usize,
        opcode: &OpCode,
        global: &str,
    ) -> Result<(), Self::Err> {
        self.visitor
            .visit_global_instruction(self.offset.get(), opcode, global)?;
        self.offset.set(self.offset.get() + 2);
        Ok(())
    }

    fn visit_local_var_instruction(
        &mut self,
        _offset: usize,
        opcode: &OpCode,
        var_idx: u8,
    ) -> Result<(), Self::Err> {
        self.visitor
            .visit_local_var_instruction(self.offset.get(), opcode, var_idx)?;
        self.offset.set(self.offset.get() + 2);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn can_drive_visitor() {}
}
