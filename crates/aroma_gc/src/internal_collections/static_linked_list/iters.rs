use std::iter::FusedIterator;
use crate::internal_collections::static_linked_list::{Link, StaticLinkedList};

/// Iterator over linked list
#[derive(Debug)]
pub struct Iter<'a, T> {
    _ref: &'a StaticLinkedList<T>,
    ptr: Link<T>,
    last: Link<T>,
    len: usize
}

impl<'a, T> Iter<'a, T> {
    pub(super) fn new(list: &'a StaticLinkedList<T>) -> Self {
        Self {
            _ref: list,
            ptr: list.front,
            last: list.back,
            len: list.len(),
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            if let Some(ptr) = self.ptr {
                let e = &(*ptr.as_ptr()).elem;

                self.ptr = if self.ptr == self.last {
                    None
                } else {
                    self.ptr.and_then(|s| (*s.as_ptr()).next)
                };
                self.len -= 1;

                Some(e)
            } else {
                None
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}

impl<'a, T> DoubleEndedIterator for Iter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        unsafe {
            if let Some(ptr) = self.last {
                let e = &(*ptr.as_ptr()).elem;

                self.last = if self.ptr == self.last {
                    self.ptr = None;
                    None
                } else {
                    self.last.and_then(|s| (*s.as_ptr()).prev)
                };
                self.len -= 1;

                Some(e)
            } else {
                None
            }
        }
    }
}

impl<'a, T> ExactSizeIterator for Iter<'a, T> {

}

/// Iterator over linked list, in reverse
#[derive(Debug)]
pub struct RevIter<'a, T> {
    _ref: &'a StaticLinkedList<T>,
    ptr: Link<T>,
}

impl<'a, T> RevIter<'a, T> {
    pub(super) fn new(list: &'a StaticLinkedList<T>) -> Self {
        Self {
            _ref: list,
            ptr: list.back
        }
    }
}

impl<'a, T> Iterator for RevIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            self.ptr.map(|ptr| {
                let node = &(*ptr.as_ptr());
                let e = &node.elem;

                self.ptr = node.prev;
                e
            })
        }
    }
}

/// Iterator over linked list
#[derive(Debug)]
pub struct IterMut<'a, T> {
    _ref: &'a mut StaticLinkedList<T>,
    ptr: Link<T>,
}

impl<'a, T> IterMut<'a, T> {
    pub(super) fn new(list: &'a mut StaticLinkedList<T>) -> Self {
        let front = list.front;
        Self {
            _ref: list,
            ptr: front
        }
    }
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            self.ptr.map(|ptr| {
                let node = &mut (*ptr.as_ptr());
                let e = &mut node.elem;

                self.ptr = node.next;
                e
            })
        }
    }
}

/// Iterator over linked list
#[derive(Debug)]
pub struct IntoIter<T> {
    list: StaticLinkedList<T>,
}

impl<T> IntoIter<T> {
    pub(super) fn new(list: StaticLinkedList<T>) -> Self {
        Self { list }
    }
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.list.pop_front()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.list.len(), Some(self.list.len()))
    }
}

impl<T> FusedIterator for IntoIter<T> {}

impl<T> ExactSizeIterator for IntoIter<T> {}

impl<T> DoubleEndedIterator for IntoIter<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.list.pop_back()
    }
}