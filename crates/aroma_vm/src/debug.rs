//! Helps with debugging

use std::io;
use std::io::{BufWriter, stdout, Write};

use crate::chunk::{Chunk, OpCode};
use crate::function::Function;

/// Responsible for disassembling bytes
#[derive(Debug)]
pub struct Disassembler;

impl Disassembler {
    /// Disassembles a function
    #[inline]
    pub fn disassemble_function(&self, func: &Function) -> io::Result<()> {
        self.disassemble_function_to(func, stdout())
    }

    /// Disassembles a function
    #[inline]
    pub fn disassemble_function_to<W: Write>(&self, func: &Function, mut w: W) -> io::Result<()> {
        self._disassemble_function_to(func, &mut w)
    }

    /// Disassembles a function
    fn _disassemble_function_to<W: Write>(&self, func: &Function, w: &mut W) -> io::Result<()> {
        let mut buffer = BufWriter::new(w);
        writeln!(buffer, "== {} ==", func.name())?;
        writeln!(buffer, "arity: {}", func.arity())?;
        for (idx, chunk) in func.chunks().iter().enumerate() {
            self.disassemble_chunk_to(chunk, &*format!("{} chunk {idx}", func.name()), &mut buffer)?;
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
        let mut offset = 0;
        while offset < chunk.len() {
            offset = self.disassemble_instruction(chunk, offset, &mut buffer)?;
        }
        buffer.flush()?;

        Ok(())
    }

    pub fn disassemble_instruction<W: Write>(
        &self,
        chunk: &Chunk,
        offset: usize,
        mut w: W,
    ) -> io::Result<usize> {
        write!(w, "{offset:04} ")?;

        if offset > 0 && chunk.lines()[offset] == chunk.lines()[offset - 1] {
            write!(w, "   | ")?;
        } else {
            write!(w, "{:4} ", chunk.lines()[offset])?;
        }

        let instruction = OpCode::try_from(chunk.code()[offset]);
        match instruction {
            Ok(opcode) => match opcode {
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
                | OpCode::Pop => self.simple_instruction(&opcode, offset, &mut w),
                OpCode::Constant | OpCode::Call => {
                    self.constant_instruction(&opcode, chunk, offset, &mut w)
                }
                OpCode::GetLocalVar | OpCode::SetLocalVar => {
                    self.local_var_instruction(&opcode, chunk, offset, &mut w)
                }
                OpCode::GetGlobalVar | OpCode::SetGlobalVar => {
                    self.constant_instruction(&opcode, chunk, offset, &mut w)
                }
                OpCode::Jump | OpCode::JumpIfFalse => {
                    self.jump_instruction(opcode.as_ref(), chunk, offset, &mut w)
                }
                _opcode => {
                    unimplemented!("Disassembly for {_opcode:?}")
                }
            },
            Err(unknown) => {
                writeln!(w, "{unknown}")?;
                Ok(offset + 1)
            }
        }
    }

    fn simple_instruction<W: Write>(
        &self,
        opcode: &OpCode,
        offset: usize,
        mut writer: W,
    ) -> io::Result<usize> {
        writeln!(writer, "{opcode}")?;
        Ok(offset + 1)
    }

    fn constant_instruction<W: Write>(
        &self,
        opcode: &OpCode,
        chunk: &Chunk,
        offset: usize,
        mut writer: W,
    ) -> io::Result<usize> {
        let constant_idx = chunk.code()[offset + 1];
        let constant = chunk.get_constant(constant_idx).unwrap_or_else(|| {
            panic!(
                "No constant at index {constant_idx} (len = {})",
                chunk.constants().len()
            )
        });
        writeln!(writer, "{opcode:<16} #{constant_idx:<6} // {constant}")?;
        Ok(offset + 2)
    }

    fn local_var_instruction<W: Write>(
        &self,
        opcode: &OpCode,
        chunk: &Chunk,
        offset: usize,
        mut writer: W,
    ) -> io::Result<usize> {
        let var_idx = chunk.code()[offset + 1];
        writeln!(writer, "{opcode:<16} #{var_idx:<6}")?;
        Ok(offset + 2)
    }

    fn jump_instruction<W: Write>(
        &self,
        name: &str,
        chunk: &Chunk,
        offset: usize,
        mut writer: W,
    ) -> io::Result<usize> {
        let jmp_distance = u16::from_be_bytes(chunk.code()[offset + 1..][..2].try_into().unwrap());
        writeln!(writer, "{name:<16} {jmp_distance:<6}")?;
        Ok(offset + 2)
    }
}

#[cfg(test)]
mod tests {
    use crate::chunk::Chunk;
    use crate::debug::Disassembler;
    use crate::types::Value;

    #[test]
    fn test_disassemble_chunk() {
        let mut chunk = Chunk::new();
        chunk.write_all(&[0, 0, 1], 1);
        chunk.add_constant(Value::Double(1.2).into());
        Disassembler
            .disassemble_chunk(&chunk, "main")
            .expect("could not write");
    }
}
