#![doc = include_str!("../README.md")]

pub mod class;
pub mod constructor;
pub mod field;
pub mod generic;
pub mod hierarchy;
pub mod functions;
pub mod type_signature;
pub mod vis;

#[cfg(test)]
mod tests {
    use crate::hierarchy::intrinsics::OBJECT_CLASS;
    use crate::hierarchy::ClassHierarchy;

    #[test]
    fn test_create_class() {
        let hierarchy = ClassHierarchy::new();
        println!("hierarchy: {:#?}", hierarchy.classes().collect::<Vec<_>>());
    }

    #[test]
    fn test_instantiate_concrete_class() {
        let hierarchy = ClassHierarchy::new();
        let inst = hierarchy
            .instantiate(&*OBJECT_CLASS, [])
            .expect("could not instantiate");
    }
}
