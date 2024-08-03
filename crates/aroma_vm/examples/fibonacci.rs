use std::time::Instant;

use aroma_vm::function;
use aroma_vm::vm::AromaVm;

fn main() {
    let fibonacci = aroma_vm::examples::fibonacci();
    let main = function!(
        name "main",
        params (),
        ret,
        variables (),
        consts { function_ref 1 utf8 "fibonacci" long 6 },
        bytecode { const(2_u8) const(0_u8) call(1_u8) ltoi ret }
    );
    let mut vm = AromaVm::new();
    vm.load(main).expect("could not add main");
    vm.load(fibonacci).expect("could not add main");
    let start = Instant::now();
    let result = vm.start("main").expect("could not run");
    println!(
        "result: {}. Calculated in {:.3} seconds",
        result,
        start.elapsed().as_secs_f64()
    );
}
