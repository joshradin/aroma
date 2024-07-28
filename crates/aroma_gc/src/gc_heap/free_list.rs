//! An auto-joining free-list

use std::collections::BTreeMap;

use strum::IntoEnumIterator;

use crate::gc_box::GcBoxHeader;
use crate::internal_collections::static_linked_list::StaticLinkedList;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
#[repr(u16)]
enum Bin {
    Bytes32 = 32,
    Bytes64 = 64,
    Bytes128 = 128,
    Bytes256 = 256,
    Bytes512 = 512,
    Bytes1024 = 1024,
    Bytes2048 = 2048,
    Bytes4096 = 4096,
    Bytes8192 = 8192,
    BytesMore,
}

impl From<usize> for Bin {
    fn from(value: usize) -> Self {
        use Bin::*;
        match value {
            ..=32 => Bytes32,
            33..=64 => Bytes64,
            65..=128 => Bytes128,
            129..=256 => Bytes256,
            257..=512 => Bytes512,
            513..=1024 => Bytes1024,
            1025..=2048 => Bytes2048,
            2049..=4096 => Bytes4096,
            4097..=8192 => Bytes8192,
            8193.. => BytesMore,
        }
    }
}

/// A free list contains a list of offsets along with its size
#[derive(Debug)]
pub struct FreeList {
    free_nodes: BTreeMap<Bin, StaticLinkedList<Node>>,
}

impl FreeList {
    /// Creates a new, empty free list.
    pub const fn new() -> Self {
        Self {
            free_nodes: BTreeMap::new(),
        }
    }

    /// Adds a free pointer to the list.
    pub fn push(&mut self, offset: usize, size: usize) {
        let bin = Bin::from(size);
        self.free_nodes
            .entry(bin)
            .or_default()
            .push_back(Node { offset, len: size });
        self.compact();
    }

    /// Compacts members of the free lis
    fn compact(&mut self) {
        let mut nodes = self
            .free_nodes
            .iter_mut()
            .flat_map(|(_, list)| list.split_at(0))
            .fold(StaticLinkedList::new(), |mut accum, next| {
                accum.append(next);
                accum
            });
        nodes.sort_by_key(|node| node.offset);

        let compacted = nodes
            .into_iter()
            .fold(StaticLinkedList::new(), |mut accum, node| {
                let mut compacted = false;
                if let Some(last) = accum.back_mut() {
                    if node.chk_adjacent(last) == Some(Adjacency::After) {
                        last.len += node.len;
                        compacted = true;
                    }
                }

                if !compacted {
                    accum.push_back(node);
                }

                accum
            });

        for node in compacted {
            let bin = node.bin();
            self.free_nodes.entry(bin).or_default().push_back(node);
        }
    }

    /// Pops an offset from the free list, returning an offset where the targeted space is
    /// `size`.
    ///
    /// If no free pointer has enough space, `None` is returned.
    pub fn pop(&mut self, size: usize) -> Option<usize> {
        let min_bin = Bin::from(size);
        for (_bin, free_list) in self.free_nodes.range_mut(min_bin..) {
            let mut best_fit = free_list
                .iter()
                .enumerate()
                .filter(|(_, node)| node.len >= size)
                .take(128)
                .collect::<Vec<_>>();

            if !best_fit.is_empty() {
                best_fit.sort_by_cached_key(|(_, node)| node.len - size);
                let (idx, best_fit_node) = best_fit.pop()?;
                let offset = best_fit_node.offset;

                let best_fit_node = free_list.remove(idx)?;
                if best_fit_node.len - size >= size_of::<GcBoxHeader>() {
                    let node_new_len = best_fit_node.len - size;
                    let new_offset = best_fit_node.offset + size;
                    self.push(new_offset, node_new_len);
                }

                return Some(offset);
            }
        }
        None
    }

    /// Gets the number of free nodes in this list
    pub fn len(&self) -> usize {
        self.free_nodes.values().map(|s| s.len()).sum()
    }

    /// Gets the total amount of space stored in this free list
    pub fn bytes(&self) -> usize {
        self.free_nodes
            .values()
            .flatten()
            .map(|node| node.len)
            .sum()
    }
}

/// A node representing an offset and the length of the free
#[derive(Debug)]
struct Node {
    offset: usize,
    len: usize,
}

impl Node {
    #[inline]
    fn front(&self) -> usize {
        self.offset
    }

    #[inline]
    fn back(&self) -> usize {
        self.offset + self.len
    }

    #[inline]
    fn bin(&self) -> Bin {
        Bin::from(self.len)
    }

    /// Checks if this node is adjacent to another node
    fn chk_adjacent(&self, other: &Self) -> Option<Adjacency> {
        if self.back() == other.front() {
            Some(Adjacency::Before)
        } else if other.back() == self.front() {
            Some(Adjacency::After)
        } else if (self.front()..self.back()).contains(&other.front())
            || (self.front()..self.back()).contains(&other.back())
        {
            Some(Adjacency::Overlap)
        } else {
            None
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
#[repr(i8)]
enum Adjacency {
    Before = -1,
    Overlap = 0,
    After = 1,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pop() {
        let mut free_list: FreeList = FreeList::new();
        free_list.push(0, 64);
        assert!(
            matches!(free_list.pop(96), None),
            "Not enough space to make this allocation"
        );
        let popped = free_list.pop(64);
        assert!(matches!(popped, Some(0)), "should get the offset 0");
        assert_eq!(free_list.len(), 0);
    }

    #[test]
    fn test_splitting_pop() {
        let mut free_list: FreeList = FreeList::new();
        free_list.push(0, 128);
        let popped = free_list.pop(32);
        assert!(matches!(popped, Some(0)), "should get the offset 0");
        assert_eq!(free_list.len(), 1);
        assert_eq!(free_list.bytes(), 128 - 32);
    }

    #[test]
    fn test_compact() {
        let mut free_list: FreeList = FreeList::new();
        free_list.push(0, 32);
        free_list.push(64, 32);
        assert_eq!(free_list.len(), 2);
        assert_eq!(free_list.bytes(), 64);
        free_list.push(32, 32);
        assert_eq!(free_list.len(), 1);
        assert_eq!(free_list.bytes(), 96);
    }
}
