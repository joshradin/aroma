use crate::FileCollection;
use std::collections::HashSet;
use std::hash::Hash;
use std::path::{Path, PathBuf};

/// Used for representing a hierarchy of files. It extends the [`FileCollection`] trait to add hierarchy
/// query and manipulation methods.
pub trait FileTree {
    type Path: AsRef<Path> + Eq + Hash;

    fn flatten(&self) -> HashSet<Self::Path>;

    fn union<F>(self, other: F) -> impl FileTree<Path = Self::Path>
    where
        Self: Sized + 'static,
        F: FileTree<Path = Self::Path> + Sized + 'static,
    {
        let mut union = UnionFileTree::default();
        union.trees.push(Box::new(self));
        union.trees.push(Box::new(other));

        union
    }
}

#[expect(refining_impl_trait)]
impl<F: FileTree> FileCollection for F {
    type Path = F::Path;

    fn files(&self) -> HashSet<Self::Path> {
        self.flatten()
    }

    fn into_file_tree(self) -> F {
        self
    }
}

#[derive(Debug)]
pub struct SimpleFileTree<P: AsRef<Path>> {
    src: P,
}

impl<P: AsRef<Path>> SimpleFileTree<P> {
    pub fn new(src: P) -> Self {
        Self { src }
    }
}

impl<P: AsRef<Path>> FileTree for SimpleFileTree<P> {
    type Path = PathBuf;

    fn flatten(&self) -> HashSet<Self::Path> {
        let mut stack = vec![self.src.as_ref().to_path_buf()];
        let mut visited = HashSet::new();
        let mut files = HashSet::new();

        while let Some(path) = stack.pop() {
            if visited.contains(&path) {
                continue;
            }
            visited.insert(path.clone());
            if path.is_file() {
                files.insert(path);
            } else if path.is_dir() {
                if let Ok(read_dir) = std::fs::read_dir(path) {
                    read_dir
                        .into_iter()
                        .filter_map(|i| i.ok())
                        .for_each(|entry| {
                            let path = entry.path();
                            stack.push(path);
                        })
                }
            }
        }

        files
    }
}

pub struct UnionFileTree<'a, P: AsRef<Path> + Eq + Hash + 'a> {
    trees: Vec<Box<dyn FileTree<Path = P> + 'a>>,
}

impl<'a, P, F> FromIterator<F> for UnionFileTree<'a, P>
where
    P: AsRef<Path> + Eq + Hash + 'a,
    F: FileTree<Path = P> + 'a,
{
    fn from_iter<T: IntoIterator<Item = F>>(iter: T) -> Self {
        Self {
            trees: iter
                .into_iter()
                .map(|f| Box::new(f) as Box<dyn FileTree<Path = P>>)
                .collect(),
        }
    }
}

impl<'a, P: AsRef<Path> + Eq + Hash + 'a> Default for UnionFileTree<'a, P>
where
    P: AsRef<Path> + Eq + Hash + 'a,
{
    fn default() -> Self {
        Self { trees: Vec::new() }
    }
}

impl<'a, P: AsRef<Path> + Eq + Hash + 'a> FileTree for UnionFileTree<'a, P> {
    type Path = P;

    fn flatten(&self) -> HashSet<Self::Path> {
        self.trees.iter().flat_map(|t| t.flatten()).collect()
    }
}
