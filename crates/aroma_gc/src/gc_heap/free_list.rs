//! An auto-joining free-list

use std::collections::BTreeMap;

use strum::{EnumIter, IntoEnumIterator};

use crate::gc_box::GcBoxHeader;
use crate::internal_collections::static_linked_list::StaticLinkedList;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, EnumIter)]
#[repr(usize)]
enum Bin {
    Bytes32,
    Bytes64,
    Bytes128,
    Bytes256,
    Bytes512,
    Bytes1024,
    Bytes2048,
    Bytes4096,
    Bytes8192,
    BytesMore,
}

impl Bin {
    pub const fn min_size(&self) -> usize {
        match self {
            Bin::Bytes32 => 0,
            Bin::Bytes64 => 33,
            Bin::Bytes128 => 65,
            Bin::Bytes256 => 129,
            Bin::Bytes512 => 257,
            Bin::Bytes1024 => 513,
            Bin::Bytes2048 => 1025,
            Bin::Bytes4096 => 2049,
            Bin::Bytes8192 => 4097,
            Bin::BytesMore => 8193,
        }
    }

    pub const fn max_size(&self) -> Option<usize> {
        match self {
            Bin::Bytes32 => Some(32),
            Bin::Bytes64 => Some(64),
            Bin::Bytes128 => Some(128),
            Bin::Bytes256 => Some(256),
            Bin::Bytes512 => Some(512),
            Bin::Bytes1024 => Some(1024),
            Bin::Bytes2048 => Some(2048),
            Bin::Bytes4096 => Some(4096),
            Bin::Bytes8192 => Some(8192),
            Bin::BytesMore => None,
        }
    }
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
    threshold: f32,
    bytes: usize,
    auto_compact: bool,
    counter: usize,
}

impl FreeList {
    /// Creates a new, empty free list. Auto compacts by default
    pub const fn new() -> Self {
        Self {
            free_nodes: BTreeMap::new(),
            threshold: 0.65,
            bytes: 0,
            auto_compact: true,
            counter: 0,
        }
    }

    /// Creates a new, empty free list.
    pub const fn with_auto_compact(auto_compact: bool) -> Self {
        Self {
            free_nodes: BTreeMap::new(),
            threshold: 0.65,
            bytes: 0,
            auto_compact,
            counter: 0,
        }
    }

    /// Adds a free pointer to the list.
    pub fn push(&mut self, offset: usize, size: usize) {
        let bin = Bin::from(size);
        self.free_nodes
            .entry(bin)
            .or_default()
            .push_back(Node { offset, len: size });

        self.bytes += size;

        if self.auto_compact && self.counter == 64 && self.fragmentation() > self.threshold {
            self.compact();
            self.counter = 0;
            let fragmentation = self.fragmentation();
            if fragmentation > self.threshold {
                self.threshold = (self.threshold + 0.10).min(0.90)
            }
        } else if self.auto_compact {
            self.counter += 1;
        }
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

        let mut compacted = nodes
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

        compacted.sort_by_key(|node| node.len);
        // reverse over the bins
        for bin in Bin::iter().rev() {
            let mut r_index: Option<usize> = None;
            for (idx, node) in compacted.iter().enumerate().rev() {
                let node_size = node.len;
                if node_size >= bin.min_size() {
                    r_index = Some(idx);
                } else {
                    break;
                }
            }

            let Some(r_index) = r_index else {
                continue;
            };

            if let Some(split) = compacted.split_at(r_index) {
                debug_assert!(
                    split.iter().all(|node| node.len as u16 >= bin as u16),
                    "not all in {split:?} >= {}",
                    bin as u16
                );
                if !split.is_empty() {
                    self.free_nodes.entry(bin).or_default().append(split);
                }
            }
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
                self.bytes -= best_fit_node.len;

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
        self.bytes
    }

    /// Try to get a quick-ish way to measure fragmentation.
    ///
    /// This value is 1 - adj/frag where adj is the amount of packets you could insert into the memory
    /// if they were all next to each other over frag which is the actual number that can be inserted
    /// in its current state. This results in a range of values \[0, 1], where the greater the value the more
    /// fragmented the free list is.
    pub fn fragmentation(&self) -> f32 {
        const FRAGMENTATION_TEST_SIZE: usize = 512;

        if self.len() == 0 {
            return 0.0;
        }

        let adj = self.bytes() as f32 / FRAGMENTATION_TEST_SIZE as f32;
        let frag = self
            .free_nodes
            .values()
            .flatten()
            .fold(0., |mut accum, node| {
                accum + (node.len as f32 / FRAGMENTATION_TEST_SIZE as f32).floor()
            });
        1.0 - frag / adj
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
        free_list.push(0, 256);
        free_list.push(256, 256);
        assert_eq!(free_list.bytes(), 512);
        free_list.push(512, 512);
        assert_eq!(free_list.bytes(), 1024);
        free_list.compact();
        assert!(
            free_list.fragmentation() <= 0.1,
            "{}",
            free_list.fragmentation()
        );
    }

    #[test]
    fn test_compact_many() {
        let mut free_list: FreeList = FreeList::with_auto_compact(false);
        for i in 0..1024 {
            free_list.push(i * 8, 8);
            free_list.push(i * 8 + 8 * 1024, 8);
        }
        assert_eq!(free_list.len(), 2048);
        free_list.compact();
        assert_eq!(free_list.len(), 1);
    }
}
