//! Provide some macros for working with aroma_vm

#[doc(hidden)]
#[cfg_attr(feature = "macros", macro_export)]
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

    (ltoi) => {
        $crate::chunk::OpCode::LtoI as u8
    };
    (itol) => {
        $crate::chunk::OpCode::IToL as u8
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

#[doc(hidden)]
#[cfg_attr(feature = "macros", macro_export)]
macro_rules! _literal_to_constant {
    (utf8 $literal:literal) => {
        $crate::chunk::Constant::Utf8($literal)
    };
    (int $literal:literal) => {
        $crate::chunk::Constant::Int($literal)
    };
    (long $literal:literal) => {
        $crate::chunk::Constant::Long($literal)
    };
    (function_ref $literal:literal) => {
        $crate::chunk::Constant::FunctionId($literal)
    };
}

#[cfg_attr(feature = "macros", macro_export)]
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

#[cfg_attr(feature = "macros", macro_export)]
macro_rules! function {
    (
        name $name:literal,
        params ($($p_ty:expr),* $(,)?),
        ret $($r_ty:expr)?,
        $($tt:tt)*
    ) => {
        {
            use $crate::types::function::ObjFunction;
            let bytecode = $crate::bytecode!($($tt)*);
            let mut ret_type = None;
            $(
                ret_type = Some($r_ty);
            )?
            ObjFunction::new($name, &[$($p_ty),*], ret_type.as_ref(),vec![bytecode])
        }
    };
}

#[cfg_attr(feature = "macros", macro_export)]
macro_rules! native {
    (fn $name:ident($($param:ident: $param_ty:ty),* $(,)?) $block:block) => {{
        use crate::types::Value;
        use crate::vm::error::VmError;

        fn $name($($param: $param_ty),*) -> Result<(), VmError>{
            $block
        }

        fn __wrapper(values: &[Value]) -> Result<Option<Value>, VmError> {
            let mut values = std::collections::VecDeque::from_iter(values.iter().cloned());
            $(
                let $param: $param_ty = values.pop_front().unwrap().try_into()?;
            )*
            $name($($param),*)?;
            Ok(None)
        }

        crate::types::function::ObjNative::new(
            stringify!($name),
            #[allow(unsed)] {
                let mut sum = 0;
                $(
                    let $param: $param_ty;
                    sum += 1;
                )*
                sum
            },
            __wrapper
        )}
    };
    (fn $name:ident($($param:ident: $param_ty:ty),* $(,)?) -> $ret:ty $block:block) => {{
        use crate::types::Value;
         use crate::vm::error::VmError;

        fn $name($($param: $param_ty),*) -> Result<$ret, VmError> {
            $block
        }

        fn __wrapper(values: &[Value]) -> Result<Option<Value>, VmError> {
            let mut values = std::collections::VecDeque::from_iter(values.iter().cloned());
            $(
                let $param: $param_ty = values.pop_front().unwrap().try_into()?;
            )*
            let raw_ty = $name($($param),*)?;
            let to_value = Value::try_from(raw_ty)?;
            Ok(Some(to_value))
        }

        crate::types::function::ObjNative::new(
            stringify!($name),
            #[allow(unsed)] {
                let mut sum = 0;
                $(
                    let $param: $param_ty;
                    sum += 1;
                )*
                sum
            },
            __wrapper
        )}
    };
}

#[cfg(not(feature = "macros"))]
pub(crate) use {_instruction_to_bytecode, _literal_to_constant, bytecode, function, native};
#[cfg(test)]
mod tests {
    use crate::debug::Disassembler;
    use crate::types::function::ObjNative;

    #[test]
    fn test_bytecode_macro() {
        let bytecode = bytecode! {
            consts {
                utf8 "hello, world!"
            }
            bytecode {
                const(0_u8)
                neg
                ret
            }
        };
        Disassembler
            .disassemble_chunk(&bytecode, "bytecode")
            .expect("could not disassemble");
    }

    #[test]
    fn test_native_no_ret_macro() {
        let native: ObjNative = native!(
            fn print(any: Value) {
                Ok(())
            }
        );
        assert_eq!(native.name(), "print");
    }

    #[test]
    fn test_native_macro() {
        let native: ObjNative = native!(
            fn sq(v: i32) -> i32 {
                Ok(v * v)
            }
        );
        assert_eq!(native.name(), "sq");
        assert_eq!(native.arity(), 1);
    }

    #[test]
    fn test_static_macro() {
        static NATIVE: ObjNative = native!(
            fn sq(v: i32) -> i32 {
                Ok(v * v)
            }
        );
        assert_eq!(NATIVE.name(), "sq");
        assert_eq!(NATIVE.arity(), 1);
    }
}
