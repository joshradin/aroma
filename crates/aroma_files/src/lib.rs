#![doc = include_str!("../README.md")]

mod file_collection;
mod file_tree;

pub use self::{file_collection::FileCollection, file_tree::FileTree};
use crate::file_collection::SimpleFileCollection;
use file_tree::SimpleFileTree;
use std::path::{Path, PathBuf};

/// Creates a file tree from a starting file
pub fn file_tree<'a>(src: &'a Path) -> impl FileTree<Path = PathBuf> + 'a {
    SimpleFileTree::new(src)
}

/// Creates a file collection from an iterator of paths
pub fn files<'a, I: IntoIterator<Item = &'a Path>>(
    src: I,
) -> impl FileCollection<Path = &'a Path> + 'a {
    SimpleFileCollection::from_iter(src)
}

/// re-exports everything
pub mod prelude {
    pub use super::*;
}

#[cfg(test)]
mod tests {
    use crate::{file_tree, FileCollection};
    use std::path::Path;

    #[test]
    fn test_file_tree() {
        let src = Path::new(env!("CARGO_MANIFEST_DIR"));
        let file_tree = file_tree(src);
        let flattened = file_tree.files().into_iter().collect::<Vec<_>>();
        assert!(flattened.len() > 0);
        assert!(flattened.iter().all(|i| i.is_file()), "all must be files");
        println!("flattened: {:#?}", flattened);
    }
}
