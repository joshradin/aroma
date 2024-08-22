use std::collections::{BTreeSet, HashMap};
use std::ops::Index;

use itertools::Itertools;
use log::trace;
use petgraph::prelude::*;
use rangemap::RangeMap;

use aroma_bytecode::chunk::{IntoOpcodeIterator, OpCode};

/// A graph of
#[derive(Debug, Default)]
pub struct OffsetGraph {
    g: DiGraph<OffsetRange, ()>,
}

impl OffsetGraph {
    pub fn node_indices(&self) -> impl Iterator<Item = NodeIndex> {
        self.g.node_indices()
    }

    pub fn neighbors(&self, node: NodeIndex) -> impl Iterator<Item = NodeIndex> + '_ {
        self.g.neighbors(node)
    }
}

impl Index<NodeIndex> for OffsetGraph {
    type Output = OffsetRange;

    fn index(&self, index: NodeIndex) -> &Self::Output {
        &self.g[index]
    }
}

/// Gets offset graphs from a bytecode slice
pub fn get_offset_graph(bytecode: &[u8]) -> OffsetGraph {
    let mut graph = OffsetGraph {
        g: Default::default(),
    };

    let mut visited = BTreeSet::new();
    let mut stack = vec![0];
    let mut blocks = RangeMap::<usize, usize>::new();
    let mut jump_from_to = HashMap::<usize, BTreeSet<usize>>::new();
    let mut cnt = 0;

    while let Some(start_offset) = stack.pop() {
        trace!("starting at offset {start_offset:x}");
        if visited.contains(&start_offset) {
            continue;
        }
        visited.insert(start_offset);

        let mut iter = bytecode[start_offset..]
            .into_opcode_iter()
            .filter(|&(offset, ..)| {
                start_offset + offset
                    < visited
                        .range(start_offset + 1..)
                        .next()
                        .copied()
                        .unwrap_or(bytecode.len())
            })
            .filter(|op| is_terminal(op.1));
        if let Some((offset, opcode, bytes)) = iter.next() {
            let end_offset = start_offset + offset + opcode.bytes();
            trace!(
                "found range {:x?} (start: {:x}, offset: {:x}, opcode_len: {})",
                start_offset..end_offset,
                start_offset,
                offset,
                opcode.bytes()
            );
            blocks.insert(start_offset..end_offset, cnt);
            cnt += 1;
            match opcode {
                OpCode::Return => { /* do nothing */ }
                OpCode::JumpIfFalse => {
                    let jump_size = u16::from_be_bytes(bytes.try_into().unwrap());
                    let next_start = end_offset + jump_size as usize;
                    jump_from_to
                        .entry(start_offset + offset)
                        .or_default()
                        .insert(next_start);
                    jump_from_to
                        .entry(start_offset + offset)
                        .or_default()
                        .insert(end_offset);
                    stack.push(next_start);
                    stack.push(end_offset);
                }
                OpCode::Jump => {
                    let jump_size = u16::from_be_bytes(bytes.try_into().unwrap());
                    let next_start = end_offset + jump_size as usize;
                    jump_from_to
                        .entry(start_offset + offset)
                        .or_default()
                        .insert(next_start);
                    stack.push(next_start);
                }
                OpCode::Loop => {
                    let jump_size =
                        (-i32::from(u16::from_be_bytes(bytes.try_into().unwrap()))) as isize;
                    let next_start = end_offset.saturating_add_signed(jump_size);
                    jump_from_to
                        .entry(start_offset + offset)
                        .or_default()
                        .insert(next_start);
                    stack.push(next_start);
                }
                _ => {}
            }
        } else if let Some(last) = visited.range(start_offset + 1..).next().copied() {
            let end_offset = last;
            trace!("found range {:x?} ", start_offset..end_offset);
            blocks.insert(start_offset..end_offset, cnt);
            cnt += 1;
        }
        trace!("stack: {stack:x?}");
    }

    trace!("blocks: {blocks:#x?}");
    trace!("jump_from_to: {jump_from_to:#x?}");

    let mut node_range_map = RangeMap::new();
    for (range, _) in blocks.iter() {
        let start = range.start;
        let end = range.end;

        node_range_map.insert(
            start..end,
            graph.g.add_node(OffsetRange {
                from: start,
                to: end,
            }),
        );
    }

    for (from, to_set) in jump_from_to {
        for to in to_set {
            let start = node_range_map.get(&from).unwrap();
            let end = node_range_map.get(&to).unwrap();
            graph.g.add_edge(*start, *end, ());
        }
    }

    for ((first_block, _), _) in blocks
        .iter()
        .sorted_by_key(|(r, _)| r.start)
        .tuple_windows()
    {
        let end = first_block.end;
        trace!("getting last opcode of block at {:?}", first_block);
        if let Some((_, last_opcode, _)) = bytecode[first_block.start..first_block.end]
            .into_opcode_iter()
            .last()
        {
            trace!("last opcode is {:?}", last_opcode);
            if !is_terminal(last_opcode) {
                let start = node_range_map.get(&first_block.start).unwrap();
                let end = node_range_map.get(&end).unwrap();
                graph.g.add_edge(*start, *end, ());
            }
        }
    }

    graph
}

fn is_terminal(instruction: OpCode) -> bool {
    matches!(
        instruction,
        OpCode::Jump | OpCode::JumpIfFalse | OpCode::Loop | OpCode::Return
    )
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub struct OffsetRange {
    /// offset of first instruction to include in this range
    pub from: usize,
    /// exclusive end of bytes
    pub to: usize,
}
