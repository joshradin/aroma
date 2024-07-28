use std::iter::FusedIterator;
use crate::internal_collections::static_linked_list::{Link, StaticLinkedList};

/// Iterator over linked list
#[derive(Debug)]
pub struct Iter<'a, T> {
    _ref: &'a StaticLinkedList<T>,
    ptr: Link<T>,
}

impl<'a, T> Iter<'a, T> {
    pub(super) fn new(list: &'a StaticLinkedList<T>) -> Self {
        Self {
            _ref: list,
            ptr: list.front
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            self.ptr.map(|ptr| {
                let node = &(*ptr.as_ptr());
                let e = &node.elem;

                self.ptr = node.next;
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