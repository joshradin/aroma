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
            jz('then)
            pop
            lget(0_u8)
            ret

            'then:
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

/// Factorial function implemented with a loop
pub fn factorial() -> ObjFunction {
    function!(
        name "factorial",
        params (Type::Long),
        ret Type::Long,
        variables (
            Type::Long,
            Type::Long,
        ),
        consts {
            long 0
            long 1
        },
        bytecode {
            lset(0_u8)
            lget(0_u8)
            const(1_u8)
            lset(1_u8) // i = 0
            const(1_u8) // sum = 1
            lset(2_u8)
            jmp('loop_end) // jump to end of
            'loop_block:
            pop
            lget(2_u8)
            lget(1_u8)
            mul
            lset(2_u8)
            lget(1_u8)
            const(1_u8)
            add
            lset(1_u8)
            'loop_end:
            lget(1_u8)
            const(1_u8)
            sub
            lget(0_u8)
            eq
            jz('loop)
            jmp('done)
            pop
            'loop:
            loop('loop_block)
            'done:
            pop
            lget(2_u8)
            ret
        }

    )
}
