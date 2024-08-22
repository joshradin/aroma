use std::mem::transmute;
use std::sync::Arc;
use std::time::{Duration, Instant};

use aroma_vm::jit::JIT;
use aroma_vm::vm::{AromaVm, StaticFunctionTable};
use aroma_vm::{examples, function};

const TEST_N: i64 = 47;

fn main() {
    let expected = quick_fib(TEST_N);
    println!("expected value of fib({}) = {}", TEST_N, expected);

    let jit_duration = run_jit(expected);
    println!("jit duration = {:.3?}", jit_duration.as_secs_f64());
    let vm_duration = run_vm(expected);
    println!("vm duration = {:.3?}", vm_duration.as_secs_f64());
}

fn run_vm(expected: i64) -> Duration {
    let fibonacci = examples::fibonacci();
    let main = function!(
        name "main",
        params (),
        ret,
        variables (),
        consts { function_ref 1 utf8 "fibonacci" long TEST_N },
        bytecode { const(2_u8) const(0_u8) call(1_u8) ltoi ret }
    );
    let mut vm = AromaVm::new();
    vm.load(main).expect("could not add main");
    vm.load(fibonacci).expect("could not add main");
    let start = Instant::now();
    let result = vm.start("main").expect("could not run");
    assert_eq!(result, expected as i32);
    start.elapsed()
}

fn run_jit(expected: i64) -> Duration {
    let fibonacci = Arc::new(examples::fibonacci());
    let static_table = StaticFunctionTable::default();
    static_table
        .write()
        .insert(fibonacci.name().to_string(), fibonacci.clone());
    let ptr = JIT::new(&static_table)
        .compile(&fibonacci)
        .expect("could not compile");
    let start = Instant::now();
    let result = unsafe { transmute::<_, fn(i64) -> i64>(ptr)(TEST_N) };
    assert_eq!(result, expected);
    start.elapsed()
}

fn quick_fib(n: i64) -> i64 {
    let mut dynamic = vec![0_i64; n as usize + 1];
    dynamic[0] = 0;
    dynamic[1] = 1;
    for i in 2..=n as usize {
        dynamic[i] = dynamic[i - 1] + dynamic[i - 2];
    }
    dynamic[n as usize]
}
