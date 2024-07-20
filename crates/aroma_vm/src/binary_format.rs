//! Contains the format for "binary" files ran by the aroma_vm

use bitfield::bitfield;

/// An aroma binary file
#[derive(Debug)]
pub struct AromaBin {
    pub header: AromaHeader,
    pub program_headers: Box<[ProgramHeader]>,
    pub section_headers: Box<[ProgramHeader]>
}

#[derive(Debug)]
pub struct AromaHeader {
    pub lang_version: usize,
    pub entry_point: u64,
    pub program_header_table_offset: u64,
    pub section_header_table_offset: u64,
    pub program_header_len: u64,
    pub program_headers: u64,
    pub section_header_len: u64,
    pub section_headers: u64
}

#[derive(Debug)]
pub struct ProgramHeader {
    pub addr: u64,
    pub file_size: u64,
    pub memory_size: u64,
}

#[derive(Debug)]
pub struct SectionHeader {
    pub name: String,
    pub kind: SectionType,
    pub flags: SectionFlags,
    pub addr: u64,
    pub size: u64
}

#[derive(Debug)]
pub enum SectionType {
    Bytecode,
    ConstantPool,
    DynamicLink
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