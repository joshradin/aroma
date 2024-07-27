use test_log::test;

use aroma_gc::GcHeap;

#[test]
fn allocate_many() {
    static ALLOCATIONS: usize = 1_000_000;
    let mut gc = GcHeap::new().unwrap();
    for i in 0..ALLOCATIONS {
        let gc = unsafe { gc.allocate(i).unwrap() };
    }
}
