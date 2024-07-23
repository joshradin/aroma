use std::cell::Cell;
use static_assertions::assert_impl_all;
use gc::GcBox;

mod gc;

/// Trait for tracing all members of an object
pub unsafe trait Trace {
    fn finalize_glue(&self) {}
    fn trace(&self) {}
}

/// A garbage collected pointer
pub struct Gc<T: Trace + 'static> {
    root: Cell<bool>,
    _ptr: *mut GcBox<T>,
}

unsafe impl<T : Send + Trace> Send for Gc<T> {

}

assert_impl_all!(Gc<usize> : Send);

unsafe impl Trace for usize {

}

