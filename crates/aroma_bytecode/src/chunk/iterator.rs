use crate::chunk::opcode::OpCode;

#[derive(Debug)]
pub struct OpcodeIterator<'a> {
    offset: usize,
    src: &'a [u8],
}

impl<'a> Iterator for OpcodeIterator<'a> {
    type Item = (usize, OpCode, &'a [u8]);

    fn next(&mut self) -> Option<Self::Item> {
        if self.src.is_empty() {
            return None;
        }
        let opcode = OpCode::try_from(self.src[0]).expect("source is not bytecode");
        let bytes_len = opcode.bytes();
        let (ret, next) = self.src.split_at(bytes_len);
        self.src = next;
        let offset = self.offset;
        self.offset += bytes_len;
        Some((offset, opcode, &ret[1..]))
    }
}

pub trait IntoOpcodeIterator<'a> {
    fn into_opcode_iter(self) -> OpcodeIterator<'a>;
}

impl<'a, T: AsRef<[u8]> + ?Sized> IntoOpcodeIterator<'a> for &'a T {
    fn into_opcode_iter(self) -> OpcodeIterator<'a> {
        OpcodeIterator {
            offset: 0,
            src: self.as_ref(),
        }
    }
}
