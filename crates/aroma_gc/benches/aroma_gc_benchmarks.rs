use std::iter;

use criterion::{criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion, Throughput};
use rand::prelude::*;

use aroma_gc::GcHeap;
use aroma_gc::__export::FreeList;

mod free_list_benches;

fn gc_throughput(c: &mut Criterion) {
    static KB: usize = 1024;

    let mut group = c.benchmark_group("gc");
    for size in [KB, 2 * KB, 4 * KB, 8 * KB, 16 * KB].iter() {
        group.throughput(Throughput::Bytes(*size as u64));
        let mut gc = GcHeap::new().unwrap();
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.iter(|| {
                iter::repeat(0_u8)
                    .take(size)
                    .try_for_each(|b| gc.allocate(b).map(|_| ()))
            });
        });
    }
    group.finish();
}

criterion_group!(
    memory,
    gc_throughput,
    free_list_benches::free_list_throughput
);
criterion_main!(memory);
