//! Helps with debugging

use std::io;
use std::io::{BufWriter, stdout, Write};
use crate::chunk::{Chunk, OpCode};

/// Responsible for disassembling bytes
#[derive(Debug)]
pub struct Disassembler;

impl Disassembler {
    /// Disassembles a chunk to a specific writer
    pub fn disassemble_chunk_to<W : Write>(&self, chunk: &Chunk, name: &str, writer: W) -> io::Result<()> {
        let mut buffer = BufWriter::new(writer);
        writeln!(buffer, "== {} ==", name)?;
        let mut offset = 0;
        while offset < chunk.len() {
            offset = self.disassemble_instruction(chunk, offset, &mut buffer)?;
        }
        buffer.flush()?;

        Ok(())
    }

    /// Disassembles a chunk
    #[inline]
    pub fn disassemble_chunk(&self, chunk: &Chunk, name: &str) -> io::Result<()>  {
        self.disassemble_chunk_to(chunk, name, &mut stdout())
    }

    pub fn disassemble_instruction<W : Write>(&self, chunk: &Chunk, offset: usize, mut w: W) -> io::Result<usize>  {
        write!(w, "{offset:04} ")?;

        if offset > 0 && chunk.lines()[offset] == chunk.lines()[offset - 1] {
            write!(w, "   | ")?;
        } else {
            write!(w, "{:4} ", chunk.lines()[offset])?;
        }

        let instruction= OpCode::try_from(chunk.code()[offset]);
        match instruction {
            Ok(opcode) => {
                match opcode {
                    OpCode::Return => {
                        self.simple_instruction(opcode.as_ref(), offset, &mut w)
                    }
                    OpCode::Constant => {
                        self.constant_instruction(opcode.as_ref(), chunk, offset, &mut w)
                    }
                }
            }
            Err(unknown) => {
                writeln!(w, "{unknown}")?;
                Ok(offset + 1)
            }
        }
    }

    fn simple_instruction<W : Write>(&self, name: &str, offset: usize, mut writer: W) -> io::Result<usize> {
        writeln!(writer, "{name}")?;
        Ok(offset + 1)
    }

    fn constant_instruction<W : Write>(&self, name: &str, chunk: &Chunk, offset: usize, mut writer: W) -> io::Result<usize> {
        let constant_idx = chunk.code()[offset + 1];
        let constant = chunk.get_constant(constant_idx).expect("Constant should be defined");
        writeln!(writer, "{name:<16} {constant}")?;
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
        chunk.add_constant(Value::Double(1.2));
        Disassembler.disassemble_chunk(&chunk, "main").expect("could not write");
    }
}