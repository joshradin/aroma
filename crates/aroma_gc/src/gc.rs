use std::fmt::{Debug, Display, Formatter, Pointer};
use std::marker::PhantomData;
use std::ops::Deref;

use crate::gc_heap::{GcBoxLink, MemoryLock, WeakMemoryLock};
use crate::{GcHeap, Trace, Tracer};

/// A garbage collected pointer
pub struct Gc<T: ?Sized + 'static> {
    pub(crate) ptr: GcBoxLink<T>,
    guard: WeakMemoryLock,
}

impl<T: ?Sized + 'static> Gc<T> {
    /// Casts this to
    pub fn cast<U: ?Sized + 'static>(self) -> Gc<U> {
        Gc {
            ptr: self.ptr.cast(),
            guard: self.guard,
        }
    }

    pub fn get(&self) -> Option<Ref<T>> {
        self.guard.upgrade().map(|guard| Ref::new(guard, self.ptr))
    }
}

unsafe impl<T: Trace + ?Sized + 'static> Trace for Gc<T> {
    unsafe fn trace<U>(&self, tracer: &U)
    where
        U: Tracer,
    {
        (*self.ptr.read().as_ptr()).trace(tracer)
    }
    // fn trace(&self, tracer: U) {
    //     unsafe {
    //         ( *self.ptr.read().as_ptr() ).trace()
    //     }
    // }
}

impl<T: ?Sized + 'static> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
            guard: self.guard.clone(),
        }
    }
}

impl<T: ?Sized + 'static + Debug> Debug for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Gc").field(&&*self.get().unwrap()).finish()
    }
}

impl<T: ?Sized + 'static> Pointer for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Object[{:p} -> {:p}]", self.ptr, unsafe {
            self.ptr.read()
        })
    }
}

impl<T: ?Sized + 'static + Display> Display for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&*self.get().unwrap(), f)
    }
}

impl<T: Trace + ?Sized + 'static> Gc<T> {
    pub(crate) fn new(ptr: GcBoxLink<T>, guard: &MemoryLock) -> Self {
        Self {
            ptr,
            guard: MemoryLock::weak(guard),
        }
    }
}

unsafe impl<T: Trace + ?Sized + Send + 'static> Send for Gc<T> {}

/// A reference to the value stored in a [`GcHeap`](GcHeap)
#[derive(Debug)]
pub struct Ref<'a, T: ?Sized + 'static> {
    ptr: GcBoxLink<T>,
    guard: MemoryLock,
    _lf: PhantomData<&'a ()>,
}

impl<'a, T: ?Sized + 'static> Drop for Ref<'a, T> {
    fn drop(&mut self) {
        self.guard.dec()
    }
}

impl<'a, T: ?Sized + 'static> Ref<'a, T> {
    pub fn new(guard: MemoryLock, ptr: GcBoxLink<T>) -> Self {
        guard.inc();
        Self {
            ptr,
            guard,
            _lf: PhantomData,
        }
    }
}

impl<T: ?Sized + 'static> Deref for Ref<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &(*(*self.ptr.as_ptr()).as_ptr()).data }
    }
}
