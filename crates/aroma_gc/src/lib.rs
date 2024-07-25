use std::alloc::Layout;
use std::fmt::{Debug, Display, Formatter, Pointer};
use std::ops::Deref;
use static_assertions::assert_impl_all;
use crate::gc_heap::GcBoxLink;

mod gc_box;
mod gc_heap;
mod static_linked_list;

/// Trait for tracing all members of an object
pub unsafe trait Trace {
    fn finalize_glue(&self) {}
    fn trace(&self) {}
}

/// A garbage collected pointer
pub struct Gc<T: Trace + ?Sized + 'static> {
    ptr: GcBoxLink<T>,
}


impl<T: Trace + ?Sized + 'static> Gc<T> {
    #[inline]
    pub fn to_trace_object(self) -> Gc<dyn Trace> {
        self.cast()
    }

    /// Casts this to
    fn cast<U : Trace + ?Sized + 'static>(self) -> Gc<U> {
        Gc { ptr: self.ptr.cast() }
    }
}

impl<T: Trace + ?Sized + 'static> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self { ptr: self.ptr }
    }
}

impl<T: Trace + ?Sized + 'static> Copy for Gc<T> {
}


impl<T: Trace + ?Sized + 'static + Debug> Debug for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T: Trace + ?Sized + 'static + Display> Display for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T: Trace + ?Sized + 'static> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(*self.ptr.as_ptr()).as_ptr() }
    }
}

impl<T: Trace + ?Sized + 'static> Gc<T> {
    pub(crate) fn new(ptr: GcBoxLink<T>) -> Self {
        Self { ptr }
    }
}

unsafe impl<T : Trace + ?Sized + Send + 'static> Send for Gc<T> {
}

assert_impl_all!(Gc<usize> : Send);

/// Copy types are always trace-able
unsafe impl<T : Copy> Trace for T {}


