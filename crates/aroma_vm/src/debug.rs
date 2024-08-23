//! Helps with debugging

use std::io;
use std::io::{stdout, BufWriter, Write};

use crate::types::function::ObjFunction;
use aroma_bytecode::chunk::Constant;
use aroma_bytecode::chunk::{Chunk, ChunkVisitor, ChunkVisitorFunctions, OpCode};

/// Responsible for disassembling bytes
#[derive(Debug)]
pub struct Disassembler;

impl Disassembler {
    /// Disassembles a function
    #[inline]
    pub fn disassemble_function(&self, func: &ObjFunction) -> io::Result<()> {
        self.disassemble_function_to(func, stdout())?;
        stdout().flush()
    }

    /// Disassembles a function
    #[inline]
    pub fn disassemble_function_to<W: Write>(
        &self,
        func: &ObjFunction,
        mut w: W,
    ) -> io::Result<()> {
        self._disassemble_function_to(func, &mut w)
    }

    /// Disassembles a function
    fn _disassemble_function_to<W: Write>(&self, func: &ObjFunction, w: &mut W) -> io::Result<()> {
        let mut buffer = BufWriter::new(w);
        writeln!(buffer, "== {} ==", func.name())?;
        writeln!(buffer, "arity: {}", func.arity())?;
        for (idx, chunk) in func.chunks().iter().enumerate() {
            self.disassemble_chunk_to(chunk, &format!("{} chunk {idx}", func.name()), &mut buffer)?;
        }
        buffer.flush()?;
        Ok(())
    }

    /// Disassembles a chunk
    #[inline]
    pub fn disassemble_chunk(&self, chunk: &Chunk, name: &str) -> io::Result<()> {
        self.disassemble_chunk_to(chunk, name, &mut stdout())
    }

    /// Disassembles a chunk to a specific writer
    pub fn disassemble_chunk_to<W: Write>(
        &self,
        chunk: &Chunk,
        name: &str,
        writer: W,
    ) -> io::Result<()> {
        let mut buffer = BufWriter::new(writer);
        writeln!(buffer, "== {} ==", name)?;
        if !chunk.constants().is_empty() {
            writeln!(buffer, "= constants =")?;
            for (idx, constant) in chunk.constants().iter().enumerate() {
                format_constant(chunk, idx as u8, constant, &mut buffer)?;
                writeln!(buffer)?;
            }
        }
        writeln!(buffer, "= bytecode =")?;
        let visitor = DisassemblerVisitor {
            w: &mut buffer,
            chunk,
        };
        chunk.visit(visitor)?;
        buffer.flush()?;

        Ok(())
    }
}

fn format_constant<W: Write>(
    chunk: &Chunk,
    idx: u8,
    constant: &Constant,
    w: &mut W,
) -> io::Result<()> {
    write!(w, "#{idx}: ")?;
    match constant {
        Constant::String(idx) => {
            write!(w, "string(")?;
            let c = chunk.get_constant(*idx).unwrap_or_else(|| {
                panic!(
                    "No constant at index {idx} (len = {})",
                    chunk.constants().len()
                )
            });
            format_constant(chunk, *idx, c, w)?;
            write!(w, ")")
        }
        Constant::FunctionId(idx) => {
            write!(w, "function_ref(")?;
            let c = chunk.get_constant(*idx).unwrap_or_else(|| {
                panic!(
                    "No constant at index {idx} (len = {})",
                    chunk.constants().len()
                )
            });
            format_constant(chunk, *idx, c, w)?;
            write!(w, ")")
        }
        cons => {
            write!(w, "{}", cons)
        }
    }
}

struct DisassemblerVisitor<'a, W: Write> {
    w: W,
    chunk: &'a Chunk,
}

impl<W: Write> ChunkVisitor for DisassemblerVisitor<'_, W> {
    type Err = io::Error;

    fn visit_opcode(&mut self, offset: usize, _: &OpCode) -> Result<(), Self::Err> {
        write!(self.w, "0x{offset:06x} ")?;

        if offset > 0 && self.chunk.lines()[offset] == self.chunk.lines()[offset - 1] {
            write!(self.w, "   | ")?;
        } else {
            write!(self.w, "{:4} ", self.chunk.lines()[offset])?;
        }
        Ok(())
    }

    fn visit_simple_instruction(
        &mut self,
        offset: usize,
        opcode: &OpCode,
    ) -> Result<(), Self::Err> {
        ChunkVisitorFunctions::visit_simple_instruction(self, offset, opcode)?;
        writeln!(self.w, "{}", opcode.as_ref())
    }

    fn visit_jump_instruction(
        &mut self,
        offset: usize,
        opcode: &OpCode,
        jmp_offset: u16,
    ) -> Result<(), Self::Err> {
        ChunkVisitorFunctions::visit_jump_instruction(self, offset, opcode, jmp_offset)?;
        let jmp_distance =
            u16::from_be_bytes(self.chunk.code()[offset + 1..][..2].try_into().unwrap()) as usize;
        writeln!(
            self.w,
            "{:<16} {jmp_distance:<6} // {:06x}",
            opcode.as_ref(),
            offset + 3 + jmp_distance
        )?;
        Ok(())
    }

    fn visit_loop_instruction(
        &mut self,
        offset: usize,
        opcode: &OpCode,
        jmp_offset: i32,
    ) -> Result<(), Self::Err> {
        ChunkVisitorFunctions::visit_loop_instruction(self, offset, opcode, jmp_offset)?;
        let jmp_distance = i32::from(u16::from_be_bytes(
            self.chunk.code()[offset + 1..][..2].try_into().unwrap(),
        )) as isize;
        writeln!(
            self.w,
            "{:<16} {jmp_distance:<6} // {:06x}",
            opcode.as_ref(),
            offset as isize + 3 - jmp_distance
        )?;
        Ok(())
    }

    fn visit_constant_instruction(
        &mut self,
        offset: usize,
        opcode: &OpCode,
        constant_idx: u8,
        constant: &Constant,
    ) -> Result<(), Self::Err> {
        ChunkVisitorFunctions::visit_constant_instruction(
            self,
            offset,
            opcode,
            constant_idx,
            constant,
        )?;
        let constant_idx = self.chunk.code()[offset + 1];
        write!(self.w, "{:<16} #{constant_idx:<5} // ", opcode.as_ref())?;
        format_constant(self.chunk, constant_idx, constant, &mut self.w)?;
        writeln!(self.w)?;
        Ok(())
    }

    fn visit_global_instruction(
        &mut self,
        offset: usize,
        opcode: &OpCode,
        global: &str,
    ) -> Result<(), Self::Err> {
        ChunkVisitorFunctions::visit_global_instruction(self, offset, opcode, global)?;
        Ok(())
    }

    fn visit_local_var_instruction(
        &mut self,
        offset: usize,
        opcode: &OpCode,
        var_idx: u8,
    ) -> Result<(), Self::Err> {
        ChunkVisitorFunctions::visit_local_var_instruction(self, offset, opcode, var_idx)?;
        let var_idx = self.chunk.code()[offset + 1];
        writeln!(self.w, "{:<16} {var_idx:<5}", opcode.as_ref())?;
        Ok(())
    }

    fn visit_closure_instruction(
        &mut self,
        offset: usize,
        opcode: &OpCode,
        idx: u8,
        constant: &Constant,
    ) -> Result<(), Self::Err> {
        ChunkVisitorFunctions::visit_closure_instruction(self, offset, opcode, idx, constant)?;
        let constant_idx = self.chunk.code()[offset + 1];
        write!(self.w, "{:<16} #{constant_idx:<5} // ", opcode.as_ref())?;
        format_constant(self.chunk, constant_idx, constant, &mut self.w)?;
        writeln!(self.w)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::debug::Disassembler;
    use crate::examples::{factorial, fibonacci};
    use aroma_bytecode::chunk::Chunk;
    use aroma_bytecode::chunk::Constant;

    #[test]
    fn test_disassemble_chunk() {
        let mut chunk = Chunk::new();
        chunk.write_all(&[0, 0, 1], 1);
        chunk.add_constant(Constant::Long(2));
        Disassembler
            .disassemble_chunk(&chunk, "main")
            .expect("could not write");
    }

    #[test]
    fn test_disassemble_recursive_function() {
        let function = fibonacci();
        Disassembler
            .disassemble_function(&function)
            .expect("could not write function");
    }

    #[test]
    fn test_disassemble_looping_function() {
        let function = factorial();
        Disassembler
            .disassemble_function(&function)
            .expect("could not write function");
    }
}
