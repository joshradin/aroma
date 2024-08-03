use std::time::Instant;

use log::LevelFilter;

use aroma_vm::examples::factorial;
use aroma_vm::function;
use aroma_vm::vm::AromaVm;

fn main() {
    env_logger::builder()
        .filter_level(LevelFilter::Trace)
        .init();
    let factorial = factorial();
    let main = function!(
        name "main",
        params (),
        ret,
        variables (),
        consts { function_ref 1 utf8 "factorial" long 10 function_ref 4 utf8 "print" },
        bytecode { const(2_u8) const(0_u8) call(1_u8) const(3_u8) call(1_u8) ret }
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
