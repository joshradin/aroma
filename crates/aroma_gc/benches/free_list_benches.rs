use criterion::{BatchSize, BenchmarkId, Criterion, Throughput};
use rand::prelude::SliceRandom;
use rand::{Rng, thread_rng};
use aroma_gc::__export::FreeList;

pub fn free_list_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("free_list");
    for size in [1000, 2000, 4000, 8000, 16_000].iter() {
        group.throughput(Throughput::Elements(*size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.iter_batched(
                || {
                    let mut free_list = FreeList::new();
                    let mut offset = 0;
                    let mut inserts = vec![];
                    for _ in 0..size {
                        let size: usize = thread_rng().gen_range(8..=4096);
                        free_list.push(offset, size);
                        inserts.push((offset, size));
                    }

                    inserts.shuffle(&mut thread_rng());

                    (free_list, inserts)
                },
                |(mut free_list, mut inserts): (FreeList, Vec<(usize, usize)>)| {
                    while let Some((_offset, size)) = inserts.pop() {
                        free_list.pop(size);
                    }
                },
                BatchSize::SmallInput,
            );
        });
    }
    group.finish();
}
