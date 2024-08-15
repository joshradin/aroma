//! A chunk of data

use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, Index, IndexMut};

pub use constant::*;
pub use iterator::*;
pub use opcode::*;
pub use visitor::*;

mod constant;
mod iterator;
mod opcode;
mod visitor;

/// A chunk of data
pub struct Chunk {
    count: usize,
    capacity: usize,
    code: Option<Box<[u8]>>,
    lines: Vec<usize>,
    constants: Vec<Constant>,
}

macro_rules! grow_capacity {
    ($c:expr) => {
        (if $c < 8 { 8 } else { $c * 2 })
    };
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.code(), f)
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.code().iter().fold(String::new(), |mut buf, next| {
                format!("{buf}{}", format!("{next:x}"))
            })
        )
    }
}

impl Chunk {
    /// Creates a new empty chunk
    pub fn new() -> Self {
        Self {
            count: 0,
            capacity: 0,
            code: None,
            lines: vec![],
            constants: vec![],
        }
    }

    /// Writes a byte to this chunk
    pub fn write(&mut self, byte: u8, line: usize) {
        if self.capacity < self.count + 1 {
            let old_capacity = self.capacity;
            self.capacity = grow_capacity!(old_capacity);
            self.grow_capacity(old_capacity);
        }
        let next = self.count;
        let Some(buffer) = self.buffer_mut() else {
            panic!("should never be reachable");
        };
        buffer[next] = byte;
        self.lines.push(line);
        self.count += 1;
    }

    /// Add a constant into the constant pool for this chunk
    pub fn add_constant(&mut self, value: Constant) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    /// Adds all the constants, returning an iterator of the indices of the added constants
    pub fn add_constants<'a, I: IntoIterator<Item = Constant, IntoIter: 'a>>(
        &'a mut self,
        values: I,
    ) -> impl Iterator<Item = usize> + '_ {
        values.into_iter().map(|value| self.add_constant(value))
    }

    /// Writes a byte to this chunk
    pub fn write_all(&mut self, bytes: &[u8], line: usize) {
        while self.capacity < self.count + bytes.len() {
            let old_capacity = self.capacity;
            self.capacity = grow_capacity!(old_capacity);
            self.grow_capacity(old_capacity);
        }
        let next = self.count;
        let Some(buffer) = self.buffer_mut() else {
            panic!("should never be reachable");
        };
        buffer[next..][..bytes.len()].copy_from_slice(bytes);
        self.count += bytes.len();
        self.lines.extend(vec![line; bytes.len()]);
    }

    /// Gets the capacity of the chunk
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Gets the written to length of the chunk
    pub fn len(&self) -> usize {
        self.count
    }

    /// Gets the constants in the constant pool of this chunk
    pub fn constants(&self) -> &[Constant] {
        &self.constants[..]
    }

    /// Gets the line for each byte
    pub fn lines(&self) -> &[usize] {
        &self.lines
    }

    /// Gets the constant at the given index using the unknown opcode size of 1 byte for referencing
    /// a pool item.
    pub fn get_constant(&self, idx: u8) -> Option<&Constant> {
        self.constants.get(idx as usize)
    }

    /// Checks if this chunk is empty
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Gets the code segment of this chunk
    pub fn code(&self) -> &[u8] {
        self.code
            .as_ref()
            .map(|s| &s[..self.count])
            .unwrap_or_else(|| &[])
    }
    fn code_mut(&mut self) -> &mut [u8] {
        self.code
            .as_mut()
            .map(|s| &mut s[..self.count])
            .unwrap_or_else(|| &mut [])
    }
    fn buffer_mut(&mut self) -> Option<&mut [u8]> {
        self.code.as_mut().map(|s| &mut **s)
    }

    fn grow_capacity(&mut self, old_capacity: usize) {
        let new_count = self.capacity;
        let taken = self.code.take();
        let new_chunk = match taken {
            None => vec![0_u8; new_count].into_boxed_slice(),
            Some(old) => {
                let mut new = vec![0_u8; new_count];
                new[..old_capacity].copy_from_slice(&old);
                new.into_boxed_slice()
            }
        };
        self.code = Some(new_chunk);
    }

    pub fn visit<V: ChunkVisitor>(&self, visitor: V) -> Result<(), V::Err> {
        let driver = ChunkVisitorDriver::new(self, visitor);
        driver.visit()
    }
}

/// Gets the index of the last opcode in this slice of bytecode
pub fn last_opcode_index(bytecode: &[u8]) -> Option<usize> {
    if bytecode.len() == 0 {
        return None;
    }
    let mut index = 0;
    loop {
        let instruction = OpCode::try_from(bytecode[index]).ok()?;
        let bytes = instruction.bytes();
        if index + bytes >= bytecode.len() {
            break;
        }
        index += bytes;
    }

    Some(index)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_array_is_zero_sized() {
        let mem: [u8; 0] = [];
        assert_eq!(std::mem::size_of_val(&mem), 0);
    }

    #[test]
    fn test_write_to_chunk() {
        let mut chunk = Chunk::new();
        chunk.write(12, 0);
        assert_eq!(chunk.len(), 1);
        assert_eq!(chunk.code()[0], 12);
    }

    #[test]
    fn test_write_all_to_chunk() {
        let mut chunk = Chunk::new();
        chunk.write_all(b"Hello, world!", 1);
        assert_eq!(chunk.len(), 13);
        assert_eq!(&chunk.code()[..], b"Hello, world!");
        println!("{chunk:}")
    }
}
