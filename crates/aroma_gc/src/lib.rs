use std::collections::HashMap;
use std::fmt::{Debug, Display, Pointer};
use std::ops::Deref;

use static_assertions::assert_impl_all;

#[cfg(feature = "derive")]
pub use aroma_gc_derive::Trace;
pub use gc_heap::{GcConfig, GcHeap};

pub use crate::gc::Gc;
pub use tracer::{Trace, Tracer};

mod gc;
mod gc_box;
mod gc_heap;
mod internal_collections;
mod tracer;

#[doc(hidden)]
pub mod __export {
    pub use super::gc_heap::FreeList;
}

assert_impl_all!(Gc<()> : Send);
