use std::fmt::{Debug, Formatter};
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use std::sync::Arc;

use parking_lot::{Mutex, MutexGuard};

use crate::gc_box::GcBoxObj;
use crate::internal_collections::static_linked_list::StaticLinkedList;

#[derive(Default, Clone)]
pub struct IndirectionTable(Arc<Mutex<StaticLinkedList<NonNull<dyn GcBoxObj>>>>);

impl IndirectionTable {
    pub fn new() -> Self {
        Self(Arc::new(Mutex::new(StaticLinkedList::new())))
    }

    pub fn lock(&self) -> IndirectionTableGuard {
        IndirectionTableGuard {
            write_guard: self.0.lock(),
        }
    }

    pub fn as_ref(&self) -> &Mutex<StaticLinkedList<NonNull<dyn GcBoxObj>>> {
        self.0.as_ref()
    }
}

impl Debug for IndirectionTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        struct Ptr<'a>(&'a NonNull<dyn GcBoxObj>);
        impl Debug for Ptr<'_> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:p} -> {:p}", self.0, unsafe { &*self.0.as_ptr() })
            }
        }

        f.debug_struct("IndirectionTable")
            .field(
                "data",
                &self.lock().iter().map(|ptr| Ptr(ptr)).collect::<Vec<_>>(),
            )
            .finish()
    }
}

pub struct IndirectionTableGuard<'a> {
    write_guard: MutexGuard<'a, StaticLinkedList<NonNull<dyn GcBoxObj>>>,
}

impl<'a> DerefMut for IndirectionTableGuard<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.write_guard
    }
}

impl Deref for IndirectionTableGuard<'_> {
    type Target = StaticLinkedList<NonNull<dyn GcBoxObj>>;

    fn deref(&self) -> &Self::Target {
        &*self.write_guard
    }
}
