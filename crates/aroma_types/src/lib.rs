#![doc = include_str!("../README.md")]

pub mod vis;
pub mod field;
pub mod class;
pub mod hierarchy;
pub mod generic;
pub mod method;
pub mod constructor;

#[cfg(test)]
mod tests {
    use crate::hierarchy::intrinsics::OBJECT_CLASS;
    use crate::hierarchy::ClassHierarchy;

    #[test]
    fn test_create_class() {
        let mut hierarchy = ClassHierarchy::new();
        println!("hierarchy: {:#?}", hierarchy.classes().collect::<Vec<_>>());
    }

    #[test]
    fn test_instantiate_concrete_class() {
        let mut hierarchy = ClassHierarchy::new();
        let inst = hierarchy.instantiate(&*OBJECT_CLASS, []).expect("could not instantiate");
    }
}
