#![doc = include_str!("../README.md")]

pub mod class;
pub mod constructor;
pub mod delegate;
pub mod field;
pub mod functions;
pub mod generic;
pub mod hierarchy;
pub mod type_signature;
pub mod vis;

#[cfg(test)]
mod tests {
    use crate::hierarchy::intrinsics::OBJECT_CLASS;
    use crate::hierarchy::Hierarchy;

    #[test]
    fn test_create_class() {
        let hierarchy = Hierarchy::new();
        println!("hierarchy: {:#?}", hierarchy.classes().collect::<Vec<_>>());
    }

    #[test]
    fn test_instantiate_concrete_class() {
        let hierarchy = Hierarchy::new();
        let inst = hierarchy
            .instantiate(&*OBJECT_CLASS, [])
            .expect("could not instantiate");
    }
}
