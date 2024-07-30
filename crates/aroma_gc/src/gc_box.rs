use crate::gc_heap::AllocError;
use crate::{Trace, Tracer};
use static_assertions::assert_obj_safe;
use std::cell::Cell;
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut};
use std::ptr::{addr_of, addr_of_mut, NonNull};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

/// A GC Box
#[derive(Debug)]
#[repr(C)]
pub struct GcBox<T: ?Sized + 'static> {
    pub(crate) header: GcBoxHeader,
    pub(crate) data: T,
}

impl<T: 'static> GcBox<T> {
    /// Tries to allocate this box at a given
    pub unsafe fn allocate(buffer: NonNull<u8>, data: T) -> Result<NonNull<Self>, AllocError> {
        let mut uninit = buffer.as_ptr() as *mut MaybeUninit<Self>;
        uninit.write(MaybeUninit::new(GcBox {
            header: GcBoxHeader::new::<T>(),
            data,
        }));

        NonNull::new((*uninit).assume_init_mut() as *mut Self).ok_or(AllocError::NullPtr)
    }
}

impl<T: ?Sized + Trace> GcBox<T> {
    /// Gets the underlying data as bytes
    pub(crate) fn data_bytes(&self) -> Box<[u8]> {
        let data_ptr = addr_of!(self.data) as *const u8;
        unsafe {
            let slice = std::slice::from_raw_parts(data_ptr, self.header.len);
            Vec::from(slice).into_boxed_slice()
        }
    }

    /// Mark this GcBox, and trace through it's data
    pub unsafe fn mark(&self, mark: bool) {
        // Mark this node
        let marked = self.header.marked.swap(mark, Ordering::Relaxed);

        // If we weren't already marked, trace through child nodes
        if marked != mark {
            self.data._cgc_mark(mark);
        }
    }

    /// Increase the root count on this GcBox.
    pub unsafe fn root(&self) {
        // // Will block during GC
        // let _modifyroots_ok = GCBOX_CHANS.rootlock.read();

        self.header.roots.fetch_add(1, Ordering::SeqCst);
    }

    /// Decrease the root count on this GcBox.
    pub unsafe fn unroot(&self) {
        // Won't block during GC

        self.header.roots.fetch_sub(1, Ordering::SeqCst);
    }

    /// Get the value form the GcBox
    pub fn value(&self) -> &T {
        &self.data
    }
}

impl<T: ?Sized + 'static> Deref for GcBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T: ?Sized + 'static> DerefMut for GcBox<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

unsafe impl<T: Send> Send for GcBox<T> {}

const MARK_MASK: usize = 1 << (usize::BITS - 1);
const ROOTS_MASK: usize = !MARK_MASK;
const ROOTS_MAX: usize = ROOTS_MASK;

// max allowed value of roots
#[derive(Debug)]
#[repr(C)]
pub(crate) struct GcBoxHeader {
    /// Number of root role `Cgc`s pointing to this box
    roots: AtomicUsize,
    /// Marked or not -- use during GC
    marked: AtomicBool,
    len: usize,
}

impl GcBoxHeader {
    #[inline]
    pub fn new<T>() -> Self {
        GcBoxHeader {
            roots: AtomicUsize::new(1),
            marked: AtomicBool::new(false),
            len: size_of::<T>(),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }
}

/// Must be implemented for *every* garbage collected allocation
pub trait GcBoxObj {
    fn header(&self) -> &GcBoxHeader;
    unsafe fn mark_value(&self, mark: bool);
    fn size_of(&self) -> usize;
}
assert_obj_safe!(GcBoxObj);

impl<T: Trace> GcBoxObj for GcBox<T> {
    fn header(&self) -> &GcBoxHeader {
        &self.header
    }

    unsafe fn mark_value(&self, mark: bool) {
        self.mark(mark)
    }

    fn size_of(&self) -> usize {
        size_of::<T>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allocate_box() {
        let mut buffer = [0_u8; 48];
        let ptr = NonNull::new(buffer.as_mut_ptr()).unwrap();
        unsafe {
            let allocated = GcBox::allocate(ptr, 10.2f64).expect("Could not allocate");
            let v = **allocated.as_ptr();
            println!("allocated: {:?}", *allocated.as_ptr());
            assert_eq!(v, 10.2);
        }
        assert!(
            buffer.iter().any(|&b| b != 0),
            "at least some value should be non-zero"
        );
        println!("buffer: {buffer:X?}");
    }
}
