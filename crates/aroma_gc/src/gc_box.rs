use std::cell::Cell;
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use crate::gc_heap::AllocError;
use crate::Trace;

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
        let mut uninit = buffer.as_ptr() as * mut MaybeUninit<Self>;
        uninit.write(MaybeUninit::new(GcBox {
            header: GcBoxHeader::new::<T>(),
            data
        }));
        println!("gc box created at {uninit:p}");

        NonNull::new((*uninit).assume_init_mut() as *mut Self).ok_or(AllocError::NullPtr)
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
    roots: Cell<usize>, // high bit is used as mark flag
    len: usize,
}

impl GcBoxHeader {
    #[inline]
    pub fn new<T>() -> Self {
        GcBoxHeader {
            roots: Cell::new(1), // unmarked and roots count
            len: std::mem::size_of::<T>(),
        }
    }



    #[inline]
    pub fn roots(&self) -> usize {
        self.roots.get() & ROOTS_MASK
    }

    #[inline]
    pub fn inc_roots(&self) {
        let roots = self.roots.get();

        // abort if the count overflows to prevent `mem::forget` loops
        // that could otherwise lead to erroneous drops
        if (roots & ROOTS_MASK) < ROOTS_MAX {
            self.roots.set(roots + 1); // we checked that this won't affect the high bit
        } else {
            panic!("roots counter overflow");
        }
    }

    #[inline]
    pub fn dec_roots(&self) {
        self.roots.set(self.roots.get() - 1); // no underflow check
    }

    #[inline]
    pub fn is_marked(&self) -> bool {
        self.roots.get() & MARK_MASK != 0
    }

    #[inline]
    pub fn mark(&self) {
        self.roots.set(self.roots.get() | MARK_MASK);
    }

    #[inline]
    pub fn unmark(&self) {
        self.roots.set(self.roots.get() & !MARK_MASK);
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }
}


impl<T : Trace + ?Sized> GcBox<T> {
    /// Marks this `GcBox` and marks through its data.
    pub(crate) unsafe fn trace_inner(&self) {
        if !self.header.is_marked() {
            self.header.mark();
            self.data.trace();
        }
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
        assert!(buffer.iter().any(|&b| b != 0), "at least some value should be non-zero");
        println!("buffer: {buffer:X?}");
    }
}