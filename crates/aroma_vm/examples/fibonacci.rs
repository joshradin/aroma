use std::time::Instant;
use aroma_vm::function;
use aroma_vm::vm::AromaVm;

fn main() {
    let fibonacci = function!(
        name "fibonacci",
        arity 1,
        consts {
            int 0
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

    );
    let main = function!(
        name "main",
        arity 0,
        consts { function_ref 1 utf8 "fibonacci" long 22 },
        bytecode { const(2_u8) const(0_u8) call(1_u8) ltoi ret }
    );
    let mut vm = AromaVm::new();
    vm.load(main).expect("could not add main");
    vm.load(fibonacci).expect("could not add main");
    let start = Instant::now();
    let result = vm.start("main").expect("could not run");
    println!("result: {}. Calculated in {:.3} seconds", result, start.elapsed().as_secs_f64());
}