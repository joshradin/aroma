use std::fmt::{Display, Formatter};
use strum::AsRefStr;

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

#[derive(Debug, Clone, thiserror::Error)]
#[error("Unknown opcode 0x{0:02X}")]
pub struct UnknownOpcode(pub u8);

/// An opcode
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Copy, AsRefStr)]
#[strum(serialize_all = "snake_case")]
#[repr(u8)]
pub enum OpCode {
    Constant,
    Return,

    Negate,
    Add,
    Subtract,
    Mult,
    Divide,

    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,

    And,
    Not,
    Or,

    Pop,
    LtoI,
    IToL,

    SetLocalVar = 32,
    GetLocalVar,
    GetGlobalVar,
    SetGlobalVar,

    /// Jump on false
    JumpIfFalse = 100,
    /// Unconditional jump
    Jump,

    /// Jump that goes backwards.
    Loop,

    /// call a function
    Call = 128,

    /// Create a closure
    Closure = 196
}

impl OpCode {
    /// bytes required for the op-code and operands
    pub fn bytes(&self) -> usize {
        match self {
            OpCode::GetLocalVar | OpCode::SetLocalVar => 2,
            OpCode::GetGlobalVar | OpCode::SetGlobalVar => 2,
            OpCode::Constant => 2,
            OpCode::Call => 2,
            OpCode::Closure => 2,
            OpCode::Jump | OpCode::JumpIfFalse => 3,
            OpCode::Loop => 3,
            _ => 1,
        }
    }
}

impl TryFrom<u8> for OpCode {
    type Error = UnknownOpcode;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use OpCode::*;

        match value {
            0 => Ok(Constant),
            1 => Ok(Return),
            2 => Ok(Negate),
            3 => Ok(Add),
            4 => Ok(Subtract),
            5 => Ok(Mult),
            6 => Ok(Divide),

            7 => Ok(Eq),
            8 => Ok(Neq),
            9 => Ok(Gt),
            10 => Ok(Gte),
            11 => Ok(Lt),
            12 => Ok(Lte),

            13 => Ok(And),
            14 => Ok(Not),
            15 => Ok(Or),

            16 => Ok(Pop),
            17 => Ok(LtoI),
            18 => Ok(IToL),

            32 => Ok(SetLocalVar),
            33 => Ok(GetLocalVar),
            34 => Ok(GetGlobalVar),
            35 => Ok(SetGlobalVar),

            100 => Ok(JumpIfFalse),
            101 => Ok(Jump),
            102 => Ok(Loop),

            128 => Ok(Call),
            196 => Ok(Closure),

            unknown => Err(UnknownOpcode(unknown)),
        }
    }
}