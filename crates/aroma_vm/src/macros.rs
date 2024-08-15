//! Provide some macros for working with aroma_vm

#[doc(hidden)]
#[cfg_attr(feature = "macros", macro_export)]
macro_rules! _instruction_to_bytecode {
    (ret) => {
        $crate::__export::chunk::OpCode::Return as u8
    };
    (const) => {
        $crate::__export::chunk::OpCode::Constant as u8
    };

    (neg) => {
        $crate::__export::chunk::OpCode::Negate as u8
    };
    (add) => {
        $crate::__export::chunk::OpCode::Add as u8
    };
    (sub) => {
        $crate::__export::chunk::OpCode::Subtract as u8
    };
    (div) => {
        $crate::__export::chunk::OpCode::Divide as u8sd
    };
    (mul) => {
        $crate::__export::chunk::OpCode::Mult as u8
    };

    (eq) => {
        $crate::__export::chunk::OpCode::Eq as u8
    };
    (neq) => {
        $crate::__export::chunk::OpCode::Neq as u8
    };
    (lt) => {
        $crate::__export::chunk::OpCode::Lt as u8
    };
    (lte) => {
        $crate::__export::chunk::OpCode::Lte as u8
    };
    (gte) => {
        $crate::__export::chunk::OpCode::Gte as u8
    };
    (gt) => {
        $crate::__export::chunk::OpCode::Gt as u8
    };

    (and) => {
        $crate::__export::chunk::OpCode::And as u8
    };
    (Or) => {
        $crate::__export::chunk::OpCode::Or as u8
    };

    (ltoi) => {
        $crate::__export::chunk::OpCode::LtoI as u8
    };
    (itol) => {
        $crate::__export::chunk::OpCode::IToL as u8
    };

    (lset) => {
        $crate::__export::chunk::OpCode::SetLocalVar as u8
    };
    (lget) => {
        $crate::__export::chunk::OpCode::GetLocalVar as u8
    };
    (gset) => {
        $crate::__export::chunk::OpCode::SetGlobalVar as u8
    };
    (gget) => {
        $crate::__export::chunk::OpCode::GetGlobalVar as u8
    };

    (jmp) => {
        $crate::__export::chunk::OpCode::Jump as u8
    };
    (jz) => {
        $crate::__export::chunk::OpCode::JumpIfFalse as u8
    };
    (loop) => {
        $crate::__export::chunk::OpCode::Loop as u8
    };

    (pop) => {
        $crate::__export::chunk::OpCode::Pop as u8
    };

    (call) => {
        $crate::__export::chunk::OpCode::Call as u8
    };
}

#[doc(hidden)]
#[cfg_attr(feature = "macros", macro_export)]
macro_rules! require_type {
    ($ty:ty, $val:expr) => {{
        let r: $ty = $val;
        r
    }};
}

#[doc(hidden)]
#[cfg_attr(feature = "macros", macro_export)]
macro_rules! _literal_to_constant {
    (utf8 $literal:literal) => {
        $crate::__export::chunk::Constant::Utf8($crate::require_type!(&'static str, $literal))
    };
    (int $literal:expr) => {
        $crate::__export::chunk::Constant::Int($crate::require_type!(i32, $literal))
    };
    (long $literal:expr) => {
        $crate::__export::chunk::Constant::Long($crate::require_type!(i64, $literal))
    };
    (function_ref $literal:literal) => {
        $crate::__export::chunk::Constant::FunctionId($crate::require_type!(u8, $literal))
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
            $($label:lifetime : )? $operator:ident $(($($operand:tt),* $(,)?))?
            )*
        }
    ) => {
        {
            use $crate::{__export::chunk::*, types::Value};
            use std::collections::HashMap;
            let mut byte_idx = 0_i16;
            let mut chunk = Chunk::new();
            let mut label_to_idx: HashMap<&'static str, i16> = HashMap::new();
            $(
                $(chunk.add_constant($crate::_literal_to_constant!($kind $constant));)*
            )?
            // creates the map
            $(
                $(
                    label_to_idx.insert(stringify!($label), byte_idx);
                )?
                byte_idx += 1;
                $(
                    $(
                    {
                        let bytes = $crate::bytecode!(@ $operand);
                        byte_idx += bytes.len() as i16;
                    }
                    )*
                )?
            )*
            byte_idx = 0;
            $(
                {
                    chunk.write($crate::_instruction_to_bytecode!($operator), line!() as usize);
                    byte_idx += 1;
                    $(
                        $(
                            {
                            let bytes = $crate::bytecode!(@ $operand, label_to_idx, byte_idx);
                            chunk.write_all(&bytes, line!() as usize);
                            byte_idx += bytes.len() as i16;
                            }
                        )*
                    )?
                }
            )*
            chunk
        }
    };
    (@ $operand:literal, $map:expr, $current_idx:expr) => {
        $operand.to_be_bytes()
    };
    (@ $label:lifetime, $map:expr, $current_idx:expr) => {
        {
            let target = $current_idx + 2;
            (*$map.get(stringify!($label)).unwrap() - target).abs().to_be_bytes()
        }
    };
    (@ $operand:literal) => {
        $operand.to_be_bytes()
    };
    (@ $label:lifetime) => {
        {
            0_i16.to_be_bytes()
        }
    };
}

#[cfg_attr(feature = "macros", macro_export)]
macro_rules! function {
    (
        name $name:literal,
        params ($($p_ty:expr),* $(,)?),
        ret $($r_ty:expr)?,
        variables ($($v_ty:expr),* $(,)?),
        $($tt:tt)*
    ) => {
        {
            use $crate::types::function::ObjFunction;
            let bytecode = $crate::bytecode!($($tt)*);
            let mut ret_type = None;
            $(
                ret_type = Some($r_ty);
            )?
            ObjFunction::new($name, &[$($p_ty),*], ret_type.as_ref(), &[$($v_ty),*], vec![bytecode])
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
            #[allow(unused)] {
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

use aroma_bytecode::chunk::Chunk;
#[cfg(not(feature = "macros"))]
pub(crate) use {
    _instruction_to_bytecode, _literal_to_constant, bytecode, function, native, require_type,
};

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
