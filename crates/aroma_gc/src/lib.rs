use std::collections::HashMap;
use std::fmt::{Debug, Display, Pointer};
use std::ops::Deref;

use static_assertions::assert_impl_all;

#[cfg(feature = "derive")]
pub use aroma_gc_derive::Trace;
pub use gc_heap::{GcConfig, GcHeap};

pub use crate::gc::Gc;

mod gc;
mod gc_box;
mod gc_heap;
mod internal_collections;

#[doc(hidden)]
pub mod __export {
    pub use super::gc_heap::FreeList;
}

/// Trait for tracing all members of an object
pub unsafe trait Trace {
    fn finalize(&mut self) {}
    fn trace(&self) {}
}

assert_impl_all!(Gc<()> : Send);

/// Allows for quickly implementing trace. Only allowed for copy types
macro_rules! impl_empty_trace {
    ($($ty:ty)*) => {
        $(
        static_assertions::assert_impl_all!($ty : Copy);
        unsafe impl $crate::Trace for $ty {}
        )*
    };
}

/// Copy types are always trace-able
impl_empty_trace! {
    ()
    u8 u16 u32 u64 usize
    i8 i16 i32 i64 isize
    f32 f64
    &'static str
    &'static std::path::Path
    char
    bool
}

unsafe impl<T: Trace> Trace for Option<T> {
    fn finalize(&mut self) {
        match self {
            None => {}
            Some(s) => s.finalize(),
        }
    }

    fn trace(&self) {
        match self {
            None => {}
            Some(s) => s.trace(),
        }
    }
}
unsafe impl<T: Trace, E: Trace> Trace for Result<T, E> {
    fn finalize(&mut self) {
        match self {
            Ok(ok) => ok.finalize(),
            Err(err) => err.finalize(),
        }
    }

    fn trace(&self) {
        match self {
            Ok(ok) => ok.trace(),
            Err(err) => err.trace(),
        }
    }
}

unsafe impl<T: Trace, const N: usize> Trace for [T; N] {
    fn finalize(&mut self) {
        for elem in self {
            elem.finalize();
        }
    }

    fn trace(&self) {
        for elem in self {
            elem.trace();
        }
    }
}

unsafe impl<K: Trace, V: Trace> Trace for HashMap<K, V> {
    fn finalize(&mut self) {
        self.drain().for_each(|(mut k, mut v)| {
            k.finalize();
            v.finalize();
        })
    }

    fn trace(&self) {
        self.iter().for_each(|(k, v)| {
            k.trace();
            v.trace();
        })
    }
}

aroma_gc_derive::derive_tuples!(1..=12);
assert_impl_all!((bool, char, i64): Trace);
