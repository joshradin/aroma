use std::alloc::Layout;
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::mem::size_of;
use std::ptr::NonNull;
use std::sync::Arc;
use std::time::Duration;
use log::{info, trace};
use parking_lot::RwLock;

use crate::gc::Gc;
use crate::gc_box::{GcBox, GcBoxHeader};
use crate::gc_heap::{AllocError, GcBoxLink, IndirectionTable, GcBoxPtr, Generation};
use crate::gc_heap::move_guard::MoveGuard;
use crate::static_linked_list::StaticLinkedList;
use crate::Trace;

macro_rules! grow_capacity {
    ($c:expr) => {
        if $c < 32 {
            32
        } else {
            $c * 2
        }
    };
}

#[derive(Debug, Clone, Copy)]
pub struct CollectionDetails {
    /// The tme spent collecting
    pub time: Duration,
    /// The amount of bytes reclaimed in this garbage collection
    pub bytes_reclaimed: usize,
}

impl PartialEq for CollectionDetails {
    fn eq(&self, other: &Self) -> bool {
        self.time.eq(&other.time)
    }
}

impl PartialOrd for CollectionDetails {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.time.partial_cmp(&other.time)
    }
}

/// Provides stats about this generation heap
#[derive(Debug)]
pub struct GenerationStats {
    /// The generation originated from
    pub generation: Generation,
    /// The number of bytes currently allocated
    pub allocated: usize,
    /// The current capacity of the heap
    pub capacity: usize,
    /// The number of resizes this heap has had
    pub resizes: usize,
    /// The current usage of the heap
    pub usage: f64,
    /// All collections performed in this generation
    pub collections: BTreeSet<CollectionDetails>,
}

#[derive(Debug)]
pub(crate) struct GenerationBumpHeap {
    generation: Generation,
    ptr: Option<NonNull<u8>>,
    bmp: usize,
    capacity: usize,
    owned_gc_ptrs: IndirectionTable,
    resizes: usize,
    move_guard: MoveGuard,
    collection_details: BTreeSet<CollectionDetails>,
}

impl Drop for GenerationBumpHeap {
    fn drop(&mut self) {
        if let Some(ptr) = self.ptr {
            unsafe {
                std::alloc::dealloc(ptr.as_ptr(), Layout::array::<u8>(self.capacity).unwrap());
            }
        }
    }
}

impl GenerationBumpHeap {
    /// Creates a new, empty generational bump
    pub const fn new(
        generation: Generation,
        gc_pointers: Arc<RwLock<StaticLinkedList<GcBoxPtr>>>,
        guard: MoveGuard,
    ) -> Self {
        Self {
            generation,
            ptr: None,
            bmp: 0,
            capacity: 0,
            owned_gc_ptrs: gc_pointers,
            resizes: 0,
            move_guard: guard,
            collection_details: BTreeSet::new(),
        }
    }

    /// Creates a new generational bump with a preset capacity
    pub fn with_capacity(
        generation: Generation,
        gc_pointers: Arc<RwLock<StaticLinkedList<GcBoxPtr>>>,
        guard: MoveGuard,
        capacity: usize,
    ) -> Result<Self, AllocError> {
        let mut heap = Self::new(generation, gc_pointers, guard);
        heap.set_capacity(capacity)?;
        Ok(heap)
    }

    /// Gets stats about this heap
    pub fn stats(&self) -> GenerationStats {
        GenerationStats {
            generation: self.generation,
            allocated: self.bmp,
            capacity: self.capacity,
            resizes: self.resizes,
            usage: if self.capacity == 0 {
                1.0
            } else {
                self.bmp as f64 / self.capacity as f64
            },
            collections: self.collection_details.clone(),
        }
    }

    /// Allocates some element within this bump heap
    pub unsafe fn allocate_elem<T: Trace + 'static>(
        &mut self,
        elem: T,
    ) -> Result<Gc<T>, AllocError> {
        let gc_box_ptr = self._allocate_elem(elem)?;
        let mut guard = self.owned_gc_ptrs.write();
        guard.push_back(gc_box_ptr);

        let gc_box_link: GcBoxLink<T> = guard
            .get_ptr(guard.len() - 1)
            .expect("should have just allocated")
            .cast();

        Ok(Gc::new(gc_box_link, &self.move_guard))
    }

    unsafe fn _allocate_elem<T: Trace + 'static>(
        &mut self,
        elem: T,
    ) -> Result<GcBoxPtr<T>, AllocError> {
        let layout = Layout::new::<GcBox<T>>();
        let gc_box_ptr = self.allocate_bytes(layout)?;

        let gc_box_ptr: GcBoxPtr<T> = GcBox::allocate(gc_box_ptr, elem)?;
        Ok(gc_box_ptr)
    }

    /// Allocates enough space for an allocation of a given size
    unsafe fn allocate_bytes(&mut self, layout: Layout) -> Result<NonNull<u8>, AllocError> {
        while layout.size() > self.capacity - self.bmp {
            self.set_capacity(grow_capacity!(self.capacity))?;
        }

        let ptr = self
            .ptr
            .expect("ptr to bytes should be set now")
            .as_ptr()
            .add(self.bmp);
        let align_offset = ptr.align_offset(layout.align());
        let total_bmp_increase = align_offset + layout.size();
        self.bmp += total_bmp_increase;

        let ptr = ptr.add(align_offset);
        let gc_box_ptr = NonNull::new_unchecked(ptr);
        Ok(gc_box_ptr)
    }

    pub fn can_allocate<T : Trace + Sized + 'static>(&self, _elem: &T) -> bool {
        let layout = Layout::new::<GcBox<T>>();
        layout.size() <= self.capacity - self.bmp
    }

    /// takes a gc from other heap and puts it in this heap
    pub unsafe fn take<T: Trace + 'static>(
        &mut self,
        elem: &Gc<T>,
        other: &mut Self,
    ) -> Result<(), AllocError> {
        if !other.contains(&elem) {
            return Err(AllocError::InvalidGenerationMove {
                old: other.generation,
                new: self.generation,
            });
        }
        if other.owned_gc_ptrs.as_ref() as *const _ != self.owned_gc_ptrs.as_ref() as *const _ {
            return Err(AllocError::GcPointerListMustBeShared);
        }

        let new_allocation = self._allocate_elem(elem.ptr.as_ptr().read().as_ptr().read().data)?;
        let link = dbg!(elem.ptr);

        let _guard = self.owned_gc_ptrs.write();
        link.as_ptr().write(new_allocation);

        Ok(())
    }

    pub unsafe fn free<T: Trace + ?Sized + 'static>(
        &mut self,
        obj: Gc<T>,
    ) -> Result<(), AllocError> {
        let ptr = *obj.ptr.as_ptr();
        self.free_ptr(ptr)
    }

    unsafe fn free_ptr<T: Trace + ?Sized + 'static>(
        &mut self,
        ptr: GcBoxPtr<T>,
    ) -> Result<(), AllocError> {
        let size = (*ptr.as_ptr()).header.len();
        if size == 0 {
            return Err(AllocError::InvalidFree);
        }

        let len = size + size_of::<GcBoxHeader>();
        let byte_ptr = ptr.as_ptr() as *mut u8;
        let mut bytes = std::slice::from_raw_parts_mut(byte_ptr, len);
        bytes.iter_mut().for_each(|b| *b = 0);
        Ok(())
    }

    /// Checks if this generation heap contains a given ptr
    pub fn contains<T: Trace + 'static>(&self, gc: &Gc<T>) -> bool {
        let Some(ptr) = self.ptr else {
            return false;
        };
        let lo = ptr.as_ptr() as usize;
        let hi = lo + self.capacity;
        (lo..hi).contains(&unsafe { (*gc.ptr.as_ptr()).as_ptr() as usize })
    }

    /// Sets the capacity of this heap
    ///
    /// If the given capacity is less than the current space allocated the buffer is only reduced to this
    /// size.
    ///
    /// Returns `Ok(<capacity>)` on success.
    fn set_capacity(&mut self, capacity: usize) -> Result<usize, AllocError> {
        trace!("setting capacity of {:?} to {}", self.generation, capacity);
        unsafe {
            let new_capacity = capacity.max(self.bmp);
            let old_capacity = self.capacity;
            match self.ptr {
                Some(old_ptr) => {
                    let old_layout = Layout::array::<u64>(old_capacity)?;
                    let new_layout = Layout::array::<u64>(new_capacity)?;
                    trace!("changing layout from {old_layout:?} to {new_layout:?}");
                    let realloc_ptr =
                        std::alloc::realloc(old_ptr.as_ptr(), old_layout, new_layout.size());
                    if realloc_ptr.is_null() {
                        return Err(AllocError::HeapCapacityAllocationFailed(
                            self.generation,
                            new_capacity,
                        ));
                    }
                    let new_ptr = NonNull::new_unchecked(realloc_ptr);
                    if old_ptr != new_ptr {
                        self.move_guard.try_lock()?;
                        let old_range = old_ptr.as_ptr()..(old_ptr.as_ptr().add(old_capacity));

                        let mut guard = self.owned_gc_ptrs.write();
                        for elem in &mut *guard {
                            let elem_ptr = elem.as_ptr();
                            if old_range.contains(&(elem_ptr as *mut u8)) {
                                let diff = (elem_ptr as *const u8).offset_from(old_ptr.as_ptr());
                                let new_elem_ptr = new_ptr.as_ptr().offset(diff) as *mut GcBox<()>;
                                let non_null = NonNull::new_unchecked(new_elem_ptr);
                                *elem = non_null;
                            }
                        }
                        self.move_guard.unlock();
                    }
                    self.capacity = new_capacity;
                    self.ptr = Some(new_ptr);
                    self.resizes += 1;
                    Ok(self.capacity)
                }
                None => {
                    let ptr = std::alloc::alloc(Layout::array::<u64>(new_capacity)?);
                    if ptr.is_null() {
                        return Err(AllocError::HeapCapacityAllocationFailed(
                            self.generation,
                            new_capacity,
                        ));
                    };
                    let non_null = NonNull::new_unchecked(ptr);
                    self.capacity = new_capacity;
                    self.bmp = 0;
                    self.ptr = Some(non_null);
                    self.resizes += 1;
                    Ok(self.capacity)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    #[test]
    fn test_allocate() {
        let mut gc_heap =
            GenerationBumpHeap::new(Generation::Eden, Default::default(), MoveGuard::new());
        unsafe {
            let gc = gc_heap.allocate_elem(11).expect("couldnt allocate");
            assert_eq!(*gc.get().unwrap(), 11);
        }
        println!("{gc_heap:#?}");
    }

    #[test]
    fn test_grow_heap() {
        let mut gc_heap =
            GenerationBumpHeap::new(Generation::Eden, Default::default(), MoveGuard::new());
        unsafe {
            let mut ptrs = vec![];
            for i in 0..1000 {
                let ptr = gc_heap
                    .allocate_elem(i)
                    .unwrap_or_else(|e| panic!("Could not allocate {i}: {e}"));
                ptrs.push(ptr);
            }
            for i in 0..1000 {
                let ptr = &ptrs[i];
                assert_eq!(*ptr.get().unwrap(), i);
            }
        }
    }

    #[test]
    fn test_grow_heap_complex_structure() {
        #[derive(Debug, Copy, Clone, PartialEq)]
        struct Test {
            byte: u8,
            long: u64,
        }
        unsafe impl Trace for Test {}

        let mut gc_heap =
            GenerationBumpHeap::new(Generation::Eden, Default::default(), MoveGuard::new());
        unsafe {
            let mut ptrs = vec![];
            for i in 0..1000 {
                let ptr = gc_heap
                    .allocate_elem(Test {
                        byte: i as u8,
                        long: i,
                    })
                    .unwrap_or_else(|e| panic!("Could not allocate {i}: {e}"));
                println!("ptrs[{i}] = {ptr:?}");
                ptrs.push(ptr);
            }

            for i in 0..1000 {
                let ptr = &*ptrs[i as usize].get().unwrap();
                assert_eq!(
                    *ptr,
                    Test {
                        byte: i as u8,
                        long: i
                    }
                );
            }
        }
    }

    #[test]
    fn test_grow_heap_variable_size() {
        let mut gc_heap =
            GenerationBumpHeap::new(Generation::Eden, Default::default(), MoveGuard::new());
        unsafe {
            let mut ptrs: Vec<Gc<dyn Trace>> = vec![];
            for i in 0..1000 {
                let ptr: Gc<dyn Trace> = if i % 2 == 0 {
                    gc_heap
                        .allocate_elem(i as u8)
                        .unwrap_or_else(|e| panic!("Could not allocate {i}: {e}"))
                        .to_trace_object()
                } else {
                    gc_heap
                        .allocate_elem(i as u16)
                        .unwrap_or_else(|e| panic!("Could not allocate {i}: {e}"))
                        .to_trace_object()
                };
                ptrs.push(ptr);
                println!("{:?}", gc_heap.stats());
            }
            for i in 0..1000 {
                let ptr = &*ptrs[i].get().unwrap();
                gc_heap.free(ptrs[i].clone()).unwrap();
                gc_heap
                    .free(ptrs[i].clone())
                    .expect_err("can't double free");
            }
        }
    }

    #[test]
    fn test_move_data_between_heaps() {
        let all_pointers: Arc<RwLock<StaticLinkedList<_>>> = Default::default();
        let mut gc_heap1 =
            GenerationBumpHeap::new(Generation::S0, all_pointers.clone(), MoveGuard::new());
        let mut gc_heap2 =
            GenerationBumpHeap::new(Generation::S1, all_pointers.clone(), MoveGuard::new());
        unsafe {
            let mut ptrs: Vec<Gc<usize>> = vec![];
            for i in 0..20 {
                let ptr = gc_heap1
                    .allocate_elem(i)
                    .unwrap_or_else(|e| panic!("Could not allocate {i}: {e}"));
                ptrs.push(ptr);
            }
            for i in 5..15 {
                let ptr = ptrs[i].clone();
                gc_heap2
                    .take(&ptr, &mut gc_heap1)
                    .expect("could not take pointer");
            }
            println!("{all_pointers:#?}");
            for i in 0..20 {
                let ptr = &ptrs[i].get().unwrap();
                assert_eq!(**ptr, i);
            }
        }
    }

    #[test]
    #[should_panic]
    #[ignore]
    fn test_move_data_reference() {
        let all_pointers: Arc<RwLock<StaticLinkedList<_>>> = Default::default();
        let mut gc_heap1 =
            GenerationBumpHeap::new(Generation::S0, all_pointers.clone(), MoveGuard::new());
        unsafe {
            let ptr = gc_heap1.allocate_elem(255).unwrap();
            let borrowed = &*ptr.get().unwrap();
            for i in 0..20 {
                gc_heap1
                    .allocate_elem(i)
                    .unwrap_or_else(|e| panic!("Could not allocate {i}: {e}"));
            }
            assert_eq!(borrowed, &255);
        }
    }
}
