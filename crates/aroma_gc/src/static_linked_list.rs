//! A statically linked list meaning that all allocations *within* the linked list never move

use std::fmt::{Debug, Formatter};
use std::iter::FusedIterator;
use std::marker::PhantomData;
use std::ptr::{addr_of, addr_of_mut, NonNull};

pub struct StaticLinkedList<T> {
    front: Link<T>,
    back: Link<T>,
    len: usize,
    _t: PhantomData<T>,
}

impl<T: Debug> Debug for StaticLinkedList<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
         .entries(self.iter().map(|s| (format!("{s:p}"), s)))
         .finish()
    }
}

impl<T> Default for StaticLinkedList<T> {
    fn default() -> Self {
        Self::new()
    }
}

unsafe impl<T: Send> Send for StaticLinkedList<T> {}

impl<T> StaticLinkedList<T> {
    /// Creates a new static linked list
    pub const fn new() -> Self {
        Self {
            front: None,
            back: None,
            len: 0,
            _t: PhantomData,
        }
    }

    /// Pushes an element to the front of the linked list
    pub fn push_front(&mut self, elem: T) {
        unsafe {
            let new = NonNull::new_unchecked(Box::into_raw(
                Box::new(Node {
                    next: None,
                    prev: None,
                    elem,
                })
            ));
            if let Some(old) = self.front {
                (*old.as_ptr()).prev = Some(new);
                (*new.as_ptr()).next = Some(old);
            } else {
                debug_assert!(self.back.is_none());
                debug_assert!(self.front.is_none());
                debug_assert!(self.len == 0);
                self.back = Some(new);
            }
            self.front = Some(new);
            self.len += 1;
        }
    }


    /// Pushes an element to the front of the linked list
    pub fn pop_front(&mut self) -> Option<T> {
        unsafe {
            self.front.map(|node| {
                let boxed_node = Box::from_raw(node.as_ptr());
                let result = boxed_node.elem;

                self.front = boxed_node.next;
                if let Some(new) = self.front {
                    (*new.as_ptr()).prev = None;
                } else {
                    debug_assert!(self.len == 1);
                    self.back = None;
                }

                self.len -= 1;
                result
            })
        }
    }

    /// Pushes an element to the back of the linked list
    pub fn push_back(&mut self, elem: T) {
        unsafe {
            let new = NonNull::new_unchecked(Box::into_raw(
                Box::new(Node {
                    next: None,
                    prev: None,
                    elem,
                })
            ));
            if let Some(old) = self.back {
                (*old.as_ptr()).next = Some(new);
                (*new.as_ptr()).prev = Some(old);
            } else {
                debug_assert!(self.back.is_none());
                debug_assert!(self.front.is_none());
                debug_assert!(self.len == 0);
                self.front = Some(new);
            }
            self.back = Some(new);
            self.len += 1;
        }
    }

    /// Pushes an element to the front of the linked list
    pub fn pop_back(&mut self) -> Option<T> {
        unsafe {
            self.back.map(|node| {
                let boxed_node = Box::from_raw(node.as_ptr());
                let result = boxed_node.elem;

                self.back = boxed_node.prev;
                if let Some(new) = self.back {
                    (*new.as_ptr()).next = None;
                } else {
                    debug_assert!(self.len == 1);
                    self.front = None;
                }

                self.len -= 1;
                result
            })
        }
    }


    /// Gets the length of the linked list
    pub fn len(&self) -> usize {
        self.len
    }

    /// Checks if this linked list is empty
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Gets the n-th node
    pub fn get(&self, index: usize) -> Option<&T> {
        if index >= self.len {
            return None;
        }

        unsafe {
            let mut ptr = self.front.expect("pointer will be not null");
            for _ in 0..index {
                ptr = (*ptr.as_ptr()).next.expect("next pointer must be defined");
            }
            Some(&(*ptr.as_ptr()).elem)
        }
    }

    /// Gets a pointer to the n-th element
    pub fn get_ptr(&self, index: usize) -> Option<NonNull<T>> {
        if index >= self.len {
            return None;
        }

        unsafe {
            let mut ptr = self.front.expect("pointer will be not null");
            for _ in 0..index {
                ptr = (*ptr.as_ptr()).next.expect("next pointer must be defined");
            }
            let elem_ptr = addr_of_mut!((*ptr.as_ptr()).elem);
            // let elem_ptr = (&mut (*ptr.as_ptr()).elem) as *mut T;
            Some(NonNull::new_unchecked(elem_ptr))
        }
    }

    /// Creates an iterator over this linked list
    pub fn iter(&self) -> Iter<T> {
        Iter {
            _ref: self,
            ptr: self.front,
        }
    }

    /// Creates a mutable iterator over this linked list
    pub fn iter_mut(&mut self) -> IterMut<T> {
        let link = self.front;
        IterMut {
            _ref: self,
            ptr: link,
        }
    }
}

type Link<T> = Option<NonNull<Node<T>>>;

#[repr(C)]
struct Node<T> {
    next: Link<T>,
    prev: Link<T>,
    elem: T,
}


impl<T> IntoIterator for StaticLinkedList<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { list: self }
    }
}


impl<T> FromIterator<T> for StaticLinkedList<T> {
    fn from_iter<I: IntoIterator<Item=T>>(iter: I) -> Self {
        let mut r = Self::new();
        for x in iter {
            r.push_back(x);
        }
        r
    }
}

/// Iterator over linked list
#[derive(Debug)]
pub struct Iter<'a, T> {
    _ref: &'a StaticLinkedList<T>,
    ptr: Link<T>,
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

impl<'a, T> IntoIterator for &'a StaticLinkedList<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Iterator over linked list
#[derive(Debug)]
pub struct IterMut<'a, T> {
    _ref: &'a mut StaticLinkedList<T>,
    ptr: Link<T>,
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

impl<'a, T> IntoIterator for &'a mut StaticLinkedList<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

/// Iterator over linked list
#[derive(Debug)]
pub struct IntoIter<T> {
    list: StaticLinkedList<T>,
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


#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use crate::static_linked_list::StaticLinkedList;

    #[test]
    fn test_push_front() {
        let mut list = StaticLinkedList::new();
        let mut expected = VecDeque::new();
        for i in 0..32 {
            list.push_front(i);
            expected.push_front(i);
        }
        assert_eq!(VecDeque::from_iter(list), expected);
    }

    #[test]
    fn test_push_back() {
        let mut list = StaticLinkedList::new();
        let mut expected = VecDeque::new();
        for i in 0..32 {
            list.push_back(i);
            expected.push_back(i);
        }
        assert_eq!(list.len(), 32);
        assert_eq!(VecDeque::from_iter(list), expected);
    }

    #[test]
    fn test_pop_front() {
        let mut list = StaticLinkedList::new();
        for i in 0..32 {
            list.push_back(i);
        }
        for i in 0..32 {
            let e = list.pop_front().unwrap();
            assert_eq!(e, i);
        }
    }

    #[test]
    fn test_pop_back() {
        let mut list = StaticLinkedList::new();
        for i in 0..32 {
            list.push_back(i);
        }
        for i in (0..32).rev() {
            let e = list.pop_back().unwrap();
            assert_eq!(e, i);
        }
    }

    #[test]
    fn test_iterator() {
        let mut list = StaticLinkedList::new();
        for i in 0..32 {
            list.push_back(i);
        }
        let mut iter = list.into_iter();
        for i in (0..32) {
            let e = iter.next().unwrap();
            assert_eq!(e, i);
        }
    }

    #[test]
    fn test_ref_iterator() {
        let mut list = StaticLinkedList::new();
        for i in 0..32 {
            list.push_back(i);
        }
        let mut iter = list.iter();
        for i in (0..32) {
            let &e = iter.next().unwrap();
            assert_eq!(e, i);
        }
    }

    #[test]
    fn test_reverse_iterator() {
        let mut list = StaticLinkedList::new();
        for i in 0..32 {
            list.push_back(i);
        }
        let mut iter = list.into_iter().rev();
        for i in (0..32).rev() {
            let e = iter.next().unwrap();
            assert_eq!(e, i);
        }
    }

    #[test]
    fn assert_pointers_static() {
        let mut list = StaticLinkedList::from_iter([1, 2, 3, 4, 5]);
        let ptr = list.get_ptr(3).unwrap();
        let expected = unsafe { *ptr.as_ptr() };
        for i in 0..4096 {
            list.push_back(i + 6);
        }
        let value = unsafe { *list.get_ptr(3).unwrap().as_ptr() };
        assert_eq!(value, expected, "Value at pointer should not have changed")
    }
}