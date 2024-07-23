//! Contains the format for "binary" files ran by the aroma_vm

use bitfield::bitfield;
use bytes::Bytes;

/// An aroma binary file
#[derive(Debug)]
pub struct AromaBin {
    pub header: AromaHeader,
    pub programs: Box<[Program]>,
    pub sections: Box<[Section]>,
}

impl AromaBin {
    /// Gets the in memory length
    pub fn read_only_memory_size(&self) -> usize {
        self.sections
            .iter()
            .filter(|sec| !sec.flags.write() && sec.flags.alloc())
            .map(|header| header.bytes.len())
            .sum::<usize>()
    }
}

/// The aroma header contains the version of the language
#[derive(Debug)]
pub struct AromaHeader {
    pub lang_version: usize,
}

/// A program just defines a start address
#[derive(Debug)]
pub struct Program {
    pub start_addr: u64,
}

#[derive(Debug)]
pub struct Section {
    pub name: String,
    pub kind: SectionType,
    pub flags: SectionFlags,
    pub offset: u64,
    pub bytes: Bytes,
}

#[derive(Debug)]
pub enum SectionType {
    Bytecode,
    ConstantPool,
    DynamicLink,
}

bitfield! {
    #[derive(Copy, Clone, Eq, PartialEq)]
    pub struct SectionFlags(u8);
    impl Debug;
    /// If this section contains writable data
    pub write, set_write: 0;
    /// if this section should always be allocated
    pub alloc, set_alloc: 1;
    /// If this section is executable
    pub exe, set_exe: 2;
}
