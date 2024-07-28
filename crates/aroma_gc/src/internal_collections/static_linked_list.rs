//! A statically linked list meaning that all allocations *within* the linked list never move.

use std::cmp::Ordering;
use std::collections::Bound;
use std::fmt::{Debug, Formatter, Pointer};
use std::iter::FusedIterator;
use std::marker::PhantomData;
use std::mem::forget;
use std::ops::RangeBounds;
use std::ptr::NonNull;

mod sort;
mod iters;
pub use iters::*;
use crate::internal_collections::static_linked_list::sort::merge_sort;

pub struct StaticLinkedList<T> {
    front: Link<T>,
    back: Link<T>,
    len: usize,
    _t: PhantomData<T>,
}

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

    /// Creates a static linked list from existing links
    unsafe fn from_raw(head: Link<T>) -> Self {
        let mut len = 0;
        let mut head = head;
        let mut tail = head;

        while let Some(ptr) = tail {
            tail = (*ptr.as_ptr()).next;
            len += 1;
        }

        Self {
            front: head,
            back: tail,
            len,
            _t: PhantomData
        }
    }

    /// Extracts the raw pointers and length from this linked list.
    ///
    /// Unsafe because the memory allocated must be now manually de-allocated or restructured via
    /// [StaticLinkedList::from_raw].
    unsafe fn to_raw(self) -> Link<T> {
        let front = self.front;

        forget(self);
        front
    }

    /// Pushes an element to the front of the linked list
    pub fn push_front(&mut self, elem: T) {
        unsafe {
            let new = NonNull::new_unchecked(Box::into_raw(Box::new(Node {
                next: None,
                prev: None,
                elem,
            })));
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
                let boxed_node = *Box::from_raw(node.as_ptr());
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

    /// Gets the front element
    pub fn front(&self) -> Option<&T> {
        self.front.map(|front| unsafe { &(*front.as_ptr()).elem })
    }

    /// Gets a mutable reference to the back element
    pub fn front_mut(&mut self) -> Option<&mut T> {
        self.front.map(|back| unsafe { &mut (*back.as_ptr()).elem })
    }

    /// Pushes an element to the back of the linked list
    pub fn push_back(&mut self, elem: T) {
        unsafe {
            let new = NonNull::new_unchecked(Box::into_raw(Box::new(Node {
                next: None,
                prev: None,
                elem,
            })));
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
                let boxed_node = *Box::from_raw(node.as_ptr());
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

    /// Gets a reference to the back element
    pub fn back(&self) -> Option<&T> {
        self.back.map(|back| unsafe { &(*back.as_ptr()).elem })
    }

    /// Gets a mutable reference to the back element
    pub fn back_mut(&mut self) -> Option<&mut T> {
        self.back.map(|back| unsafe { &mut (*back.as_ptr()).elem })
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

    /// Tries to remove the element at the given index
    pub fn remove(&mut self, index: usize) -> Option<T> {
        if index >= self.len {
            return None;
        }
        let mut r = self.split_at(index)?;
        let elem = r.pop_front();
        self.append(r);
        elem
    }

    /// Gets a pointer to the n-th element
    pub fn get_ptr(&self, index: usize) -> Option<NonNull<T>> {
        if index >= self.len {
            return None;
        }

        unsafe {
            let mut ptr = self.front.expect("pointer will be not null");
            for _i in 0..index {
                ptr = (*ptr.as_ptr()).next.unwrap_or_else(|| {
                    panic!("expected .next to be defined because index({_i}) < len({}), but .next is None: {:?}", self.len, &*ptr.as_ptr())
                })
            }
            // let elem_ptr = addr_of_mut!((*ptr.as_ptr()).elem);
            let elem_ptr = &mut (*ptr.as_ptr()).elem as *mut _;
            Some(NonNull::new_unchecked(elem_ptr))
        }
    }

    /// Creates an iterator over this linked list
    pub fn iter(&self) -> Iter<T> {
        Iter::new(self)
    }

    /// Creates a mutable iterator over this linked list
    pub fn iter_mut(&mut self) -> IterMut<T> {
        IterMut::new(self)
    }

    /// Splits the linked list at the given index, returning Some(new_list) starting at the given
    /// index if it's in bounds
    ///
    pub fn split_at(&mut self, index: usize) -> Option<Self> {
        if index > self.len {
            None
        } else if index == self.len {
            Some(StaticLinkedList::new())
        } else if index == 0 {
            Some(std::mem::replace(self, StaticLinkedList::new()))
        } else {
            unsafe {
                let this_len = index;
                let split_len = self.len - index;
                let split_tail = self.back;
                let mut split_head = self.front.unwrap();
                for _ in 0..index {
                    split_head = (*split_head.as_ptr()).next.unwrap();
                }
                let new_self_tail = (*split_head.as_ptr()).prev;

                (*split_head.as_ptr()).prev = None;
                if let Some(new_self_tail) = new_self_tail {
                    (*new_self_tail.as_ptr()).next = None;
                }

                let split_list = Self {
                    front: Some(split_head),
                    back: split_tail,
                    len: split_len,
                    _t: Default::default(),
                };

                self.len = this_len;
                self.back = new_self_tail;
                Some(split_list)
            }
        }
    }

    /// Tries to split the list at the first index `predicate` matches
    pub fn split_when<F>(&mut self, predicate: F) -> Option<Self>
    where
        F: Fn(&T) -> bool,
    {
        let index = self.iter().position(|elem| predicate(elem))?;
        self.split_at(index)
    }

    /// Tries to remove the element from the list at the first index `predicate` matches
    pub fn remove_when<F>(&mut self, predicate: F) -> Option<T>
    where
        F: Fn(&T) -> bool,
    {
        let index = self.iter().position(|elem| predicate(elem))?;
        self.remove(index)
    }

    /// Concatenates a linked list to the end of this list
    pub fn append(&mut self, other: Self) {
        if self.is_empty() {
            *self = other;
        } else if !other.is_empty() {
            let original_back = self.back.unwrap();
            let other_front = other.front.unwrap();
            unsafe {
                (*other_front.as_ptr()).prev = Some(original_back);
                (*original_back.as_ptr()).next = Some(other_front);
                self.back = other.back;
                self.len += other.len;
                forget(other);
            }
        }
    }

    #[inline]
    fn split_midway(&mut self) -> Self {
        self.split_at(self.len / 2).expect("Index out of bounds")
    }

    /// Sorts the linked list by a given key
    pub fn sort_by<F>(&mut self, compare: F)
    where
        F: FnMut(&T, &T) -> Ordering,
    {
        unsafe {
            let raw = std::mem::replace(self, Self::new()).to_raw();
            let head = merge_sort(raw, &mut { compare });
            std::mem::swap(&mut StaticLinkedList::from_raw(head), self);
        }
    }

    /// Sorts the linked list by a given key
    pub fn sort_by_key<K, F>(&mut self, f: F)
    where
        F: Fn(&T) -> K,
        K: Ord,
    {
        self.sort_by(|a, b| f(a).cmp(&f(b)))
    }

    /// Sorts this by the natural order of the elements in this node
    #[inline]
    pub fn sort(&mut self)
    where
        T: Ord,
    {
        self.sort_by(|l, r| l.cmp(r))
    }

    /// Removes the specified range
    ///
    /// # Panic
    /// Panics if start point is greater than the end point or the end point is greater than
    /// the length of the list
    pub fn drain<R: RangeBounds<usize>>(&mut self, range: R) -> Drain<T> {
        if self.is_empty() {
            return Drain {
                ptr: None,
                first: None,
                last: None,
                _ref: self,
            };
        }

        let start_idx = match range.start_bound() {
            Bound::Included(&i) => i,
            Bound::Excluded(&i) => i + 1,
            Bound::Unbounded => 0,
        };
        let end_idx = match range.end_bound() {
            Bound::Included(&i) => i,
            Bound::Excluded(&i) => i - 1,
            Bound::Unbounded => self.len() - 1,
        };

        if start_idx > end_idx {
            panic!("start index can not be greater than the end point");
        } else if end_idx >= self.len() {
            panic!("end index can not the length of the list");
        }

        let mut link = self.front;
        let mut idx = 0;
        let mut first: Link<T> = None;
        let mut last: Link<T> = None;

        loop {
            if let Some(ptr) = link {
                if idx == start_idx {
                    first = Some(ptr);
                    break;
                }

                link = unsafe { (*ptr.as_ptr()).next };
                idx += 1;
            }
        }

        let mut link = self.back;
        let mut idx = self.len - 1;
        loop {
            if let Some(ptr) = link {
                if idx == end_idx {
                    last = Some(ptr);
                    break;
                }

                link = unsafe { (*ptr.as_ptr()).prev };
                idx -= 1;
            }
        }

        unsafe {
            let prev: Link<T> = first.and_then(|ptr| (*ptr.as_ptr()).prev);
            let next: Link<T> = last.and_then(|ptr| (*ptr.as_ptr()).next);

            if let Some(first) = first {
                (*first.as_ptr()).prev = None;
            }
            if let Some(last) = last {
                (*last.as_ptr()).next = None;
            }

            if let Some(prev) = prev {
                (*prev.as_ptr()).next = next;
            }
            if let Some(next) = next {
                (*next.as_ptr()).prev = prev;
            }

            if self.front == first {
                self.front = next;
            }
            if self.back == last {
                self.back = prev;
            }

            self.len -= (end_idx - start_idx) + 1;
        }

        Drain {
            ptr: first,
            first,
            last,
            _ref: self,
        }
    }
}

impl<T: Clone> Clone for StaticLinkedList<T> {
    fn clone(&self) -> Self {
        Self::from_iter(self.iter().cloned())
    }
}

impl<T: Debug> Debug for StaticLinkedList<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter().map(|s| s)).finish()
    }
}

impl<T> Default for StaticLinkedList<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Extend<T> for StaticLinkedList<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {}
}

unsafe impl<T: Send> Send for StaticLinkedList<T> {}

impl<T> Drop for StaticLinkedList<T> {
    fn drop(&mut self) {
        let mut link = std::mem::replace(&mut self.front, None);
        self.back = None;
        self.len = 0;
        while let Some(ptr) = link {
            let boxed = unsafe { *Box::from_raw(ptr.as_ptr()) };
            link = boxed.next;
        }
    }
}

type Link<T> = Option<NonNull<Node<T>>>;

struct Node<T> {
    next: Link<T>,
    prev: Link<T>,
    elem: T,
}

impl<T> Debug for Node<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node")
            .field("next", &self.next)
            .field("prev", &self.prev)
            .finish_non_exhaustive()
    }
}

impl<T> IntoIterator for StaticLinkedList<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter::new(self)
    }
}

impl<T> FromIterator<T> for StaticLinkedList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut r = Self::new();
        for x in iter {
            r.push_back(x);
        }
        r
    }
}

impl<T: PartialEq> PartialEq for StaticLinkedList<T> {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().eq(other.iter())
    }

    fn ne(&self, other: &Self) -> bool {
        self.len() != other.len() || self.iter().ne(other.iter())
    }
}

impl<T: Eq> Eq for StaticLinkedList<T> {}

impl<T: PartialOrd> PartialOrd for StaticLinkedList<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.iter().partial_cmp(other)
    }
}

impl<T: Ord> Ord for StaticLinkedList<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.iter().cmp(other)
    }
}

impl<'a, T> IntoIterator for &'a StaticLinkedList<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut StaticLinkedList<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

/// Drains from a [StaticLinkedList]
#[derive(Debug)]
pub struct Drain<'a, T> {
    ptr: Link<T>,
    first: Link<T>,
    last: Link<T>,
    _ref: &'a mut StaticLinkedList<T>,
}

impl<'a, T> Iterator for Drain<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.ptr.map(|ptr| unsafe {
            let elem = *Box::from_raw(ptr.as_ptr());

            if Some(ptr) != self.last {
                self.ptr = elem.next;
            } else {
                self.ptr = None;
            }

            elem.elem
        })
    }
}

impl<T> Drop for Drain<'_, T> {
    fn drop(&mut self) {
        while let Some(_next) = self.next() {}
    }
}

#[cfg(test)]
mod tests {
    use std::cmp::Reverse;
    use std::collections::VecDeque;
    use std::iter::Rev;
    use super::*;

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
    fn test_split_0() {
        let mut list = StaticLinkedList::from_iter(0..12);
        let other = list.split_at(0).unwrap();
        assert_eq!(other.len(), 12);
        assert!(list.is_empty());
    }

    #[test]
    fn test_split_len() {
        let mut list = StaticLinkedList::from_iter(0..12);
        let other = list.split_at(list.len()).unwrap();
        assert_eq!(list.len(), 12);
        assert!(other.is_empty());
    }

    #[test]
    fn test_split_len_plus_1() {
        let mut list = StaticLinkedList::from_iter(0..12);
        let other = list.split_at(list.len() + 1);
        assert!(matches!(other, None));
    }

    #[test]
    fn test_split_midway() {
        let mut list = StaticLinkedList::from_iter(0..12);
        let other = list.split_at(list.len() / 2).unwrap();
        assert_eq!(list.len(), 6);
        assert_eq!(list.iter().copied().collect::<Vec<_>>(), [0, 1, 2, 3, 4, 5]);
        assert_eq!(other.len(), 6);
        assert_eq!(
            other.iter().copied().collect::<Vec<_>>(),
            [6, 7, 8, 9, 10, 11]
        );
    }

    #[test]
    fn test_split_when() {
        let mut list = StaticLinkedList::from_iter(0..12);
        let other = list.split_when(|&elem| elem == 3).unwrap();
        assert_eq!(list.len(), 3);
        assert!(matches!(other.front(), Some(3)));
        assert_eq!(other.len(), 9);
    }

    #[test]
    fn test_append() {
        let mut list = StaticLinkedList::from_iter(0..12);
        let expected = list.clone();
        let other = list.split_at(list.len() / 2).unwrap();
        list.append(other);
        println!("{list:?}");
        assert_eq!(list, expected);
    }

    #[test]
    fn test_remove() {
        let mut list = StaticLinkedList::from_iter(0..12);
        let removed = list.remove(4).unwrap();
        assert_eq!(removed, 4);
        assert_eq!(list.len(), 11);
    }

    #[test]
    fn test_drain() {
        let mut list = StaticLinkedList::from_iter(0..12);
        let removed = list.drain(3..9).collect::<StaticLinkedList<_>>();
        assert_eq!(removed, StaticLinkedList::from_iter([3, 4, 5, 6, 7, 8]));
        assert_eq!(list, StaticLinkedList::from_iter([0, 1, 2, 9, 10, 11]));
    }

    #[test]
    fn test_drain_all() {
        let mut list = StaticLinkedList::from_iter(0..12);
        let removed = list.clone().drain(..).collect::<StaticLinkedList<_>>();
        assert_eq!(removed, StaticLinkedList::from_iter(0..12));
        assert_eq!(list, StaticLinkedList::from_iter(0..12));
    }

    #[test]
    fn test_sort_natural() {
        let mut list = StaticLinkedList::from_iter((0..12).rev().chain(12..24));
        list.sort();
        assert_eq!(list, StaticLinkedList::from_iter(0..24));
    }

    #[test]
    fn test_sort_compare() {
        let mut list = StaticLinkedList::from_iter((0..12).rev().chain(12..24));
        list.sort_by(|l, r| l.cmp(r));
        assert_eq!(list, StaticLinkedList::from_iter(0..24));
    }

    #[test]
    fn test_sort_by_key() {
        let mut list = StaticLinkedList::from_iter((0..12).rev().chain(12..24));
        list.sort_by_key(|&k| Reverse(k));
        assert_eq!(list, StaticLinkedList::from_iter((0..24).rev()));
    }

    #[test]
    fn test_pointers_static() {
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
