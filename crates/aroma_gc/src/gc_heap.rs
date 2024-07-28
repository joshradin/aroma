//! Used for storing heap objects

use std::alloc::LayoutError;
use std::collections::{BTreeSet, HashMap};
use std::ptr::NonNull;
use std::sync::{Arc, LazyLock};
use std::time::Duration;

use parking_lot::RwLock;
use sysinfo::System;

pub(crate) use memory_lock::{MemoryLock, WeakMemoryLock};

use crate::gc::Gc;
use crate::gc_box::{GcBox, GcBoxHeader};
use crate::gc_heap::generation_bump_heap::{GenerationBumpHeap, GenerationStats};
use crate::gc_heap::memory_lock::MoveGuardLockError;
use crate::internal_collections::static_linked_list::StaticLinkedList;
use crate::Trace;

mod generation_bump_heap;
mod memory_lock;
mod free_list;

/// The generation
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub(crate) enum Generation {
    Eden,
    S0,
    S1,
    Tenured,
}

pub type GcBoxPtr<T = dyn Trace> = NonNull<GcBox<T>>;
pub type GcBoxLink<T = dyn Trace> = NonNull<GcBoxPtr<T>>;


fn _debug_gc_box_ptr<T : Trace + ?Sized>(ptr: GcBoxPtr<T>) {
    #[derive(Debug)]
    struct GcBoxData {
        header: GcBoxHeader,
        data: Box<[u8]>
    }

    unsafe {
        let debug = GcBoxData {
            header: (*ptr.as_ptr()).header.clone(),
            data:(*ptr.as_ptr()).data_bytes()
        };

        println!("{debug:#?}");
    }
}

macro_rules! debug_gc_box_ptr {
    ($ptr:expr) => {
        #[cfg(debug_assertions)] {
            $crate::gc_heap::_debug_gc_box_ptr($ptr);
        }
    };
}

pub(crate) use debug_gc_box_ptr;

pub(crate) type IndirectionTable = Arc<RwLock<StaticLinkedList<GcBoxPtr>>>;


/// A garbage collecting heap
///
/// Designed based on the HotSpot garbage collector.
pub struct GcHeap {
    young_gen: RwLock<GenerationBumpHeap>,
    s0: RwLock<GenerationBumpHeap>,
    s1: RwLock<GenerationBumpHeap>,
    tenured: RwLock<GenerationBumpHeap>,
    all: IndirectionTable,
    move_guard: MemoryLock,
    gc_config: GcConfig,
}

macro_rules! get_generation {
    ($self:expr, $gen:expr) => {
        match $gen {
            Generation::Eden => $self.young_gen.read(),
            Generation::S0 => $self.s0.read(),
            Generation::S1 => $self.s1.read(),
            Generation::Tenured => $self.tenured.read(),
        }
    };
    (mut, $self:expr, $gen:expr) => {
        match $gen {
            Generation::Eden => $self.young_gen.write(),
            Generation::S0 => $self.s0.write(),
            Generation::S1 => $self.s1.write(),
            Generation::Tenured => $self.tenured.write(),
        }
    };
}

impl GcHeap {

    /// Creates a new GcHeap with the default GcConfig
    #[inline]
    pub fn new() -> Result<Self, AllocError> {
        Self::with_config(GcConfig::default())
    }

    /// Creates a new gc heap from a given config
    pub fn with_config(config: GcConfig) -> Result<Self, AllocError> {
        let all: IndirectionTable = Arc::new(Default::default());
        let guard = MemoryLock::new();

        Ok(Self {
            young_gen: RwLock::new(GenerationBumpHeap::with_capacity(
                Generation::Eden,
                all.clone(),
                guard.clone(),
                config.young_size(),
            )?),
            s0: RwLock::new(GenerationBumpHeap::with_capacity(
                Generation::S0,
                all.clone(),
                guard.clone(),
                config.survivor_size(),
            )?),
            s1: RwLock::new(GenerationBumpHeap::with_capacity(
                Generation::S1,
                all.clone(),
                guard.clone(),
                config.survivor_size(),
            )?),
            tenured: RwLock::new(GenerationBumpHeap::with_capacity(
                Generation::Tenured,
                all.clone(),
                guard.clone(),
                config.tenured_size(),
            )?),
            all,
            move_guard: guard,
            gc_config: config,
        })
    }

    /// Allocates within the young generation for immediacy
    pub fn allocate<T: Trace + 'static>(&mut self, data: T) -> Result<Gc<T>, AllocError> {
        if !self.young_gen.read().can_allocate(&data) {
            todo!("garbage collection");
            if !self.young_gen.read().can_allocate(&data) {
                return Err(AllocError::HeapExhausted(self.stats().used()));
            }
        }
        unsafe { self.young_gen.write().allocate_elem(data) }
    }

    pub(crate) fn current_generation<T: Trace + 'static>(&self, gc: &Gc<T>) -> Option<Generation> {
        if self.young_gen.read().contains(gc) {
            Some(Generation::Eden)
        } else if self.s0.read().contains(gc) {
            Some(Generation::S0)
        } else if self.s1.read().contains(gc) {
            Some(Generation::S1)
        } else if self.tenured.read().contains(gc) {
            Some(Generation::Tenured)
        } else {
            None
        }
    }

    pub(crate) unsafe fn move_gc<T: Trace + 'static>(
        &mut self,
        gc: &Gc<T>,
        generation: Generation,
    ) -> Result<(), AllocError> {
        let Some(current_generation) = self.current_generation(gc) else {
            return Err(AllocError::UnrelatedAllocation);
        };
        // cant move current generation is "older" or equal to current generation

        if current_generation >= generation {
            return Err(AllocError::InvalidGenerationMove {
                old: current_generation,
                new: generation,
            });
        }

        let mut old = get_generation!(mut, self, current_generation);
        let mut new = get_generation!(mut, self, current_generation);

        new.take(gc, &mut *old)?;

        Ok(())
    }

    /// Gets the move guard for this heap
    pub fn guard(&self) -> &MemoryLock {
        &self.move_guard
    }

    /// Gets stats about this gc heap at this point in time
    pub fn stats(&self) -> GcStats {
        let young_stats = self.young_gen.read().stats();
        let s0_stats = self.s0.read().stats();
        let s1_stats = self.s1.read().stats();
        let tenured_stats = self.tenured.read().stats();

        [young_stats, s0_stats, s1_stats, tenured_stats]
            .into_iter()
            .fold(GcStats::new(), |mut accum, generation| {
                accum
                    .pauses
                    .extend(generation.collections.iter().map(|cd| cd.time));
                accum.capacity += generation.capacity;
                accum.used += generation.allocated;

                accum
                    .generation_stats
                    .insert(generation.generation, generation);
                accum
            })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum AllocError {
    #[error("Given GC pointer is not owned by this heap")]
    UnrelatedAllocation,
    #[error("Can not move a pointer from {old:?} to {new:?}")]
    InvalidGenerationMove { old: Generation, new: Generation },
    #[error("Failed to allocate {1} bytes for {0:?}")]
    HeapCapacityAllocationFailed(Generation, usize),
    #[error(transparent)]
    LayoutError(#[from] LayoutError),
    #[error("Attempted to dereference a null pointer")]
    NullPtr,
    #[error("Invalid free")]
    InvalidFree,
    #[error("Gc pointer list is not shared")]
    GcPointerListMustBeShared,
    #[error(transparent)]
    MoveGuardLockError(#[from] MoveGuardLockError),
    #[error("Can not insert new allocations on this heap. {0} bytes used.")]
    HeapExhausted(usize),
}

fn heap_contains<T: ?Sized>(heap: &[u8], ptr: *const T) -> bool {
    let range = heap.as_ptr_range();
    range.contains(&(ptr as *const u8))
}

/// Used for configuring the gc heap
#[derive(Debug, Copy, Clone)]
pub struct GcConfig {
    /// The threshold required for the heap to be considered "full" and require a garbage collection.
    ///
    /// The threshold is calculated based on the `space used/space reserved`.
    pub threshold: f64,
    /// The minimum size of the heap
    pub min_size: usize,
    /// The max size of the heap.
    ///
    /// If not set the heap can grow indefinitely.
    pub max_size: Option<usize>,
    /// The ratio between the young and tenured heap sizes.
    ///
    /// For example, a ratio of 3 means a 1:3 split, such that the combined space of eden, s0, and s1, takes up
    /// 1/4 of the total heap with the tenured taking up 3/4.
    pub new_ratio: usize,
    /// The ratio between the eden space and the s0 and s1 heaps.
    ///
    /// For example, a ratio of 6 means a 1:6 split, such that each sX heap is 1/6 the size of the eden generation.
    pub survivor_ratio: usize,
}

impl GcConfig {
    fn tenured_size(&self) -> usize {
        let parts = self.new_ratio + 1;
        let size_per_part = self.min_size / parts;
        self.new_ratio * size_per_part
    }
    #[inline]
    fn young_size(&self) -> usize {
        self.min_size - self.tenured_size()
    }
    fn eden_size(&self) -> usize {
        let parts = self.survivor_ratio + 2;
        let size_per_part = self.young_size() / parts;
        self.survivor_ratio * size_per_part
    }
    #[inline]
    fn survivor_size(&self) -> usize {
        (self.young_size() - self.eden_size()) / 2
    }
}

impl Default for GcConfig {
    fn default() -> Self {
        Self {
            threshold: 0.7,
            min_size: get_min_heap_size(),
            max_size: Some(get_max_heap_size()),
            new_ratio: 2,
            survivor_ratio: 8,
        }
    }
}

pub(crate) static SYS_INFO: LazyLock<System> = LazyLock::new(|| {
    let mut system = System::new_all();
    system.refresh_all();
    system
});

fn get_min_heap_size() -> usize {
    if !sysinfo::IS_SUPPORTED_SYSTEM {
        panic!("current os is not supported");
    }
    let physical_memory = SYS_INFO.total_memory();
    let upper_bound = physical_memory.min(bytesize::gib(1_u64)) / 64;
    let lower_bound = bytesize::mib(8_u64);
    lower_bound.max(upper_bound) as usize
}

/// Tries to get the max heap size
fn get_max_heap_size() -> usize {
    if !sysinfo::IS_SUPPORTED_SYSTEM {
        panic!("current os is not supported");
    }
    let physical_memory = SYS_INFO.total_memory();

    if physical_memory <= bytesize::mib(192_u64) {
        physical_memory as usize / 2
    } else {
        physical_memory as usize / 4
    }
}

#[derive(Debug)]
pub struct GcStats {
    pauses: BTreeSet<Duration>,
    capacity: usize,
    used: usize,
    generation_stats: HashMap<Generation, GenerationStats>,
}

impl GcStats {
    fn new() -> Self {
        Self {
            pauses: Default::default(),
            capacity: 0,
            used: 0,
            generation_stats: Default::default(),
        }
    }

    pub fn pauses(&self) -> &BTreeSet<Duration> {
        &self.pauses
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    pub fn used(&self) -> usize {
        self.used
    }
}

#[cfg(test)]
mod tests {
    use crate::{GcConfig, GcHeap};

    #[test]
    fn test_reasonable_defaults() {
        let defaults = GcConfig::default();
        assert!(defaults.threshold > 0.3);
        assert!(defaults.min_size <= defaults.max_size.unwrap());
        println!("{defaults:#?}")
    }

    #[test]
    fn test_create_heap() {
        assert!(
            GcHeap::with_config(GcConfig::default()).is_ok(),
            "could not create gc heap"
        );
    }
}
