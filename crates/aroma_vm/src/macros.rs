//! Provide some macros for working with aroma_vm

#[macro_export]
#[doc(hidden)]
macro_rules! _instruction_to_bytecode {
    (ret) => {
        $crate::chunk::OpCode::Return as u8
    };
    (const) => {
        $crate::chunk::OpCode::Constant as u8
    };

    (neg) => {
        $crate::chunk::OpCode::Negate as u8
    };
    (add) => {
        $crate::chunk::OpCode::Add as u8
    };
    (sub) => {
        $crate::chunk::OpCode::Subtract as u8
    };
    (div) => {
        $crate::chunk::OpCode::Divide as u8sd
    };
    (mul) => {
        $crate::chunk::OpCode::Mult as u8
    };

    (eq) => {
        $crate::chunk::OpCode::Eq as u8
    };
    (neq) => {
        $crate::chunk::OpCode::Neq as u8
    };
    (lt) => {
        $crate::chunk::OpCode::Lt as u8
    };
    (lte) => {
        $crate::chunk::OpCode::Lte as u8
    };
    (gte) => {
        $crate::chunk::OpCode::Gte as u8
    };
    (gt) => {
        $crate::chunk::OpCode::Gt as u8
    };

    (and) => {
        $crate::chunk::OpCode::And as u8
    };
    (Or) => {
        $crate::chunk::OpCode::Or as u8
    };

    (lset) => {
        $crate::chunk::OpCode::SetLocalVar as u8
    };
    (lget) => {
        $crate::chunk::OpCode::GetLocalVar as u8
    };
    (gset) => {
        $crate::chunk::OpCode::SetGlobalVar as u8
    };
    (gget) => {
        $crate::chunk::OpCode::GetGlobalVar as u8
    };

    (jmp) => {
        $crate::chunk::OpCode::Jump as u8
    };
    (jz) => {
        $crate::chunk::OpCode::JumpIfFalse as u8
    };

    (pop) => {
        $crate::chunk::OpCode::Pop as u8
    };


    (call) => {
        $crate::chunk::OpCode::Call as u8
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! _literal_to_constant {
    (utf8 $literal:literal) => {
        $crate::chunk::Constant::Utf8($literal)
    };
    (int $literal:literal) => {
        $crate::chunk::Constant::Int($literal)
    };
    (function_ref $literal:literal) => {
        $crate::chunk::Constant::FunctionId($literal)
    };
}

#[macro_export]
macro_rules! bytecode {
    (
        $(consts {
            $($kind:ident $constant:expr $(,)?)*
        } $(,)?)?
        bytecode {
            $(
            $operator:ident $(($($operand:expr),* $(,)?))?
            )*
        }
    ) => {
        {
            use $crate::{chunk::Chunk, types::Value};
            let mut chunk = Chunk::new();
            $(
                $(chunk.add_constant($crate::_literal_to_constant!($kind $constant));)*
            )?
            $(
                chunk.write($crate::_instruction_to_bytecode!($operator), line!() as usize);
                $(
                    $(
                        chunk.write_all(&$operand.to_be_bytes(), line!() as usize);
                    )*
                )?
            )*
            chunk
        }
    };
}

#[macro_export]
macro_rules! function {
    (
        name $name:literal,
        arity $arity:literal,
        $($tt:tt)*
    ) => {
        {
            use $crate::{function::Function};
            let bytecode = $crate::bytecode!($($tt)*);
            Function::new($name, $arity, vec![bytecode])
        }
    };
}


#[cfg(test)]
mod tests {
    use crate::debug::Disassembler;

    #[test]
    fn test_bytecode_macro() {
        let bytecode = bytecode! {
            consts {
                "hello, world!"
            }
            bytecode {
                const(0)
                neg
                ret
            }
        };
        Disassembler
            .disassemble_chunk(&bytecode, "bytecode")
            .expect("could not disassemble");
    }
}
