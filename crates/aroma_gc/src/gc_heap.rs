//! Used for storing heap objects

use std::alloc::LayoutError;
use std::ptr::NonNull;
use std::sync::{Arc};
use parking_lot::RwLock;
use crate::{Gc, Trace};
use crate::gc_box::GcBox;
use crate::gc_heap::generation_bump_heap::GenerationBumpHeap;
use crate::static_linked_list::StaticLinkedList;

mod generation_bump_heap;

/// The generation
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub(crate) enum Generation {
    Young,
    S0,
    S1,
    Tenured,
}

pub type GcBoxPtr<T = dyn Trace> = NonNull<GcBox<T>>;
pub type GcBoxLink<T = dyn Trace> = NonNull<GcBoxPtr<T>>;

pub(crate) type GcBoxMainList = Arc<RwLock<StaticLinkedList<GcBoxPtr>>>;

/// Responsible for holding all objects. Designed based on the
pub(crate) struct GcHeap {
    young_gen: RwLock<GenerationBumpHeap>,
    s0: RwLock<GenerationBumpHeap>,
    s1: RwLock<GenerationBumpHeap>,
    tenured: RwLock<GenerationBumpHeap>,
    all: GcBoxMainList,
}

macro_rules! get_generation {
    ($self:expr, $gen:expr) => {
         match $gen {
            Generation::Young => { $self.young_gen.read() }
            Generation::S0 => { $self.s0.read() }
            Generation::S1 => { $self.s1.read() }
            Generation::Tenured => { $self.tenured.read() }
        }
    };
    (mut, $self:expr, $gen:expr) => {
         match $gen {
            Generation::Young => { $self.young_gen.write() }
            Generation::S0 => { $self.s0.write() }
            Generation::S1 => { $self.s1.write() }
            Generation::Tenured => { $self.tenured.write() }
        }
    };
}

impl GcHeap {
    pub fn new() -> Self {
        let all: GcBoxMainList = Arc::new(Default::default());
        Self {
            young_gen: RwLock::new(GenerationBumpHeap::new(Generation::Young, all.clone())),
            s0: RwLock::new(GenerationBumpHeap::new(Generation::S0, all.clone())),
            s1: RwLock::new(GenerationBumpHeap::new(Generation::S1, all.clone())),
            tenured: RwLock::new(GenerationBumpHeap::new(Generation::Tenured, all.clone())),
            all,
        }
    }

    /// Allocates within the young generation for immediacy
    pub fn allocate<T: Trace + 'static>(&mut self, data: T) -> Result<Gc<T>, AllocError> {
        unsafe {
            self.young_gen.write().allocate_elem(data)
        }
    }

    pub(crate) fn current_generation<T: Trace + 'static>(&self, gc: &Gc<T>) -> Option<Generation> {
        if self.young_gen.read().contains(gc) {
            Some(Generation::Young)
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



    pub(crate) unsafe fn move_gc<T: Trace + 'static>(&mut self, gc: &Gc<T>, generation: Generation) -> Result<(), AllocError> {
        let Some(current_generation) = self.current_generation(gc) else {
            return Err(AllocError::UnrelatedAllocation);
        };
        // cant move current generation is "older" or equal to current generation

        if current_generation >= generation {
            return Err(AllocError::InvalidGenerationMove { old: current_generation, new: generation });
        }

        let mut old = get_generation!(mut, self, current_generation);
        let mut new = get_generation!(mut, self, current_generation);

        new.take(*gc, &mut *old)?;

        Ok(())
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
}

fn heap_contains<T: ?Sized>(heap: &[u8], ptr: *const T) -> bool {
    let range = heap.as_ptr_range();
    range.contains(&(ptr as *const u8))
}

#[derive(Debug)]
pub struct GcConfig {}

