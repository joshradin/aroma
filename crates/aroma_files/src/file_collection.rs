//! file collection trait

use crate::file_tree::{SimpleFileTree, UnionFileTree};
use crate::FileTree;
use std::collections::HashSet;
use std::hash::Hash;
use std::path::Path;

/// A [`FileCollection`](FileCollection) represents a collection of file system locations which can
/// be queried in certain ways
pub trait FileCollection {
    type Path: AsRef<Path> + Eq + Hash;

    /// Gets the files of this set
    fn files(&self) -> HashSet<Self::Path>;

    /// Returns true if this collection is empty
    fn is_empty(&self) -> bool {
        self.files().into_iter().next().is_none()
    }

    /// Asserts this file collection is a single file
    fn get_single_file(&self) -> Option<Self::Path> {
        let mut all = self.files().into_iter().collect::<Vec<_>>();
        if all.len() == 1 {
            Some(all.remove(0))
        } else {
            None
        }
    }

    /// Gets this FileCollection as a file tree
    fn into_file_tree(self) -> impl FileTree
    where
        Self: Sized,
        <Self as FileCollection>::Path: 'static
    {
        self.files()
            .into_iter()
            .map(|i| SimpleFileTree::new(i))
            .collect::<UnionFileTree<_>>()
    }
}

#[derive(Debug, Default)]
pub struct SimpleFileCollection<P : AsRef<Path> + Eq + Hash> {
    paths: HashSet<P>,
}

impl<P : AsRef<Path> + Eq + Hash + Clone> FromIterator<P> for SimpleFileCollection<P> {
    fn from_iter<T: IntoIterator<Item = P>>(iter: T) -> Self {
        Self {
            paths: iter.into_iter().collect(),
        }
    }
}

impl<P : AsRef<Path> + Eq + Hash + Clone> FileCollection for SimpleFileCollection<P> {
    type Path = P;

    fn files(&self) -> HashSet<Self::Path> {
        self.paths.clone()
    }
}

impl<P: AsRef<Path> + Eq + Hash + Clone> FileCollection for HashSet<P> {
    type Path = P;

    fn files(&self) -> HashSet<Self::Path> {
        self.clone()
    }
}
