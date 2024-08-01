use crate::function;
use crate::types::function::ObjFunction;
use crate::types::Type;

/// Creates the fibonacci function
pub fn fibonacci() -> ObjFunction {
    function!(
        name "fibonacci",
        params (Type::Long),
        ret Type::Long,
        variables (),
        consts {
            long 0
            long 1
            long 2
            function_ref 4
            utf8 "fibonacci"
        },
        bytecode {
            lset(0_u8)
            lget(0_u8)
            const(2_u8)
            lt
            jz(4_u16)
            pop
            lget(0_u8)
            ret

            pop
            lget(0_u8)
            const(1_u8)
            sub
            const(3_u8)
            call(1_u8)

            lget(0_u8)
            const(2_u8)
            sub
            const(3_u8)
            call(1_u8)
            add
            ret
        }

    )
}
