use std::iter;

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main, Throughput};

use aroma_gc::GcHeap;

fn throughput(c: &mut Criterion) {
    static KB: usize = 1024;

    let mut group = c.benchmark_group("alloc");
    for size in [KB, 2 * KB, 4 * KB, 8 * KB, 16 * KB].iter() {
        group.throughput(Throughput::Bytes(*size as u64));
        let mut gc = GcHeap::new().unwrap();
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.iter(|| {
                iter::repeat(0_u8).take(size).try_for_each(|b| {
                    gc.allocate(b).map(|_| ())
                })
            });
        });
    }
    group.finish();
}

criterion_group!(memory, throughput);
criterion_main!(memory);
