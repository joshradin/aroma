use aroma_vm::function;
use aroma_vm::vm::AromaVm;
use std::time::Instant;
use aroma_vm::types::Type;

fn main() {
    let factorial = function!(
            name "factorial",
            params (Type::Int),
            ret Type::Int,
            consts {
                int 0
                int 1
                utf8 "factorial"
                function_ref 2
            }
            bytecode {
                lset(0_u8)
                lget(0_u8)
                const(0_u8)
                eq
                jz(4_u16)
                pop
                const(1_u8)
                ret
                pop
                lget(0_u8)
                const(1_u8)
                sub
                const(3_u8)
                call(1_u8)
                lget(0_u8)
                mul
                ret
            }
        );
    let main = function!(
        name "main",
        params (),
        ret,
        consts { function_ref 1 utf8 "factorial" int 10 },
        bytecode { const(2_u8) const(0_u8) call(1_u8) ret }
    );
    let mut vm = AromaVm::new();
    vm.load(main).expect("could not add main");
    vm.load(factorial).expect("could not add main");
    let start = Instant::now();
    let result = vm.start("main").expect("could not run");
    println!(
        "result: {}. Calculated in {:.3} seconds",
        result,
        start.elapsed().as_secs_f64()
    );
}
