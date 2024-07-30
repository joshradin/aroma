//! Responsible for tracing

use std::collections::HashMap;

use static_assertions::assert_impl_all;

/// Responsible with tracing through [Trace]-able objects
pub trait Tracer {
    unsafe fn traverse<T: Trace>(&self, obj: &T);
}

/// Trait for tracing all members of an object
pub unsafe trait Trace {
    /// [Tracer::traverse] should be called on every collectable element of the object in question implementing
    /// [Trace].
    unsafe fn trace<T>(&self, tracer: &T)
    where
        T: Tracer;

    unsafe fn _cgc_mark(&self, mark: bool) {
        struct MarkTracer(bool);
        impl Tracer for MarkTracer {
            #[inline(always)]
            unsafe fn traverse<T: Trace>(&self, obj: &T) {
                obj._cgc_mark(self.0)
            }
        }
        self.trace(&MarkTracer(mark));
    }
    unsafe fn _cgc_root(&self) {
        struct RootTracer;
        impl Tracer for RootTracer {
            #[inline(always)]
            unsafe fn traverse<T: Trace>(&self, obj: &T) {
                obj._cgc_root()
            }
        }
        self.trace(&RootTracer);
    }
    unsafe fn _cgc_unroot(&self) {
        struct UnrootTracer;
        impl Tracer for UnrootTracer {
            #[inline(always)]
            unsafe fn traverse<T: Trace>(&self, obj: &T) {
                obj._cgc_unroot()
            }
        }
        self.trace(&UnrootTracer);
    }

    fn finalize(&mut self) {}
}

unsafe impl<T: Trace> Trace for Option<T> {
    unsafe fn trace<U: Tracer>(&self, tracer: &U) {
        match self {
            None => {}
            Some(s) => s.trace(tracer),
        }
    }

    fn finalize(&mut self) {
        match self {
            None => {}
            Some(s) => s.finalize(),
        }
    }
}

unsafe impl<U> Trace for &'static U {
    unsafe fn trace<T: Tracer>(&self, _: &T) {}
}
unsafe impl<T: Trace, E: Trace> Trace for Result<T, E> {
    unsafe fn trace<U: Tracer>(&self, tracer: &U) {
        match self {
            Ok(ok) => ok.trace(tracer),
            Err(err) => err.trace(tracer),
        }
    }

    fn finalize(&mut self) {
        match self {
            Ok(ok) => ok.finalize(),
            Err(err) => err.finalize(),
        }
    }
}

unsafe impl<T: Trace, const N: usize> Trace for [T; N] {
    unsafe fn trace<U: Tracer>(&self, tracer: &U) {
        for elem in self {
            elem.trace(tracer);
        }
    }

    fn finalize(&mut self) {
        for elem in self {
            elem.finalize();
        }
    }
}

unsafe impl<K: Trace, V: Trace> Trace for HashMap<K, V> {
    unsafe fn trace<U: Tracer>(&self, tracer: &U) {
        self.iter().for_each(|(k, v)| {
            k.trace(tracer);
            v.trace(tracer);
        })
    }

    fn finalize(&mut self) {
        self.drain().for_each(|(mut k, mut v)| {
            k.finalize();
            v.finalize();
        })
    }
}

/// Allows for quickly implementing trace. Only allowed for copy types
macro_rules! impl_empty_trace {
    ($($ty:ty)*) => {
        $(
        static_assertions::assert_impl_all!($ty : Copy);
        unsafe impl $crate::Trace for $ty {
            unsafe fn trace<U : Tracer>(&self, tracer: &U) {
            }
        }
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

aroma_gc_derive::derive_tuples!(1..=12);
assert_impl_all!((bool, char, i64): Trace);
