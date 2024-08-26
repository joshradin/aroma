//! The type hierarchy for type querying

use crate::class::{AsClassRef, Class, ClassInst, ClassRef};
use crate::generic::{GenericParameterBound, GenericParameterBounds};
use intrinsics::*;
use itertools::Itertools;
use parking_lot::RwLock;
use petgraph::prelude::*;
use std::collections::{HashMap, HashSet, VecDeque};
use std::ops::Index;

pub mod intrinsics;

/// The type hierarchy.
#[derive(Debug)]
pub struct ClassHierarchy {
    graph: DiGraph<ClassRef, GenericParameterBounds>,
    class_ref_map: HashMap<ClassRef, (NodeIndex, Class)>,
    base_class: ClassRef,
    instantiated: RwLock<HashSet<ClassInst>>,
}

impl Default for ClassHierarchy {
    fn default() -> Self {
        Self::new()
    }
}

impl ClassHierarchy {
    /// Creates a new class hierarchy with all intrinsic classes already inserted.
    pub fn new() -> Self {
        let mut hierarchy = Self {
            graph: DiGraph::new(),
            class_ref_map: Default::default(),
            base_class: OBJECT_CLASS.get_ref(),
            instantiated: Default::default(),
        };
        hierarchy.unchecked_insert(OBJECT_CLASS.clone());
        hierarchy.unchecked_insert(CLASS_CLASS.clone());
        hierarchy
            .insert(ARRAY_CLASS.clone())
            .expect("could not insert array class");
        hierarchy
            .insert(I32_CLASS.clone())
            .expect("could not insert int class");
        hierarchy
            .insert(I64_CLASS.clone())
            .expect("could not insert long class");
        hierarchy
            .insert(STRING_CLASS.clone())
            .expect("could not insert string class");
        hierarchy
    }

    /// Tries to insert a class into this hierarchy. Makes sure all referenced classes are
    /// already present within the hierarchy, failing if any is missing.
    ///
    /// If successful, a class ref is returned
    pub fn insert(&mut self, class: Class) -> Result<ClassRef> {
        if let Some(super_class) = class.super_class() {
            if !self.contains(super_class.as_ref()) {
                return Err(Error::ClassNotDefined(super_class.as_ref().clone()));
            }
        } else {
            return Err(Error::AllClassesMustHaveParent(class.get_ref()));
        }
        for mixin in class.mixins() {
            if !self.contains(mixin.as_ref()) {
                return Err(Error::ClassNotDefined(mixin.as_ref().clone()));
            }
        }
        for field in class.fields() {
            if !self.contains(field.kind().as_ref()) {
                return Err(Error::ClassNotDefined(field.kind().as_ref().clone()));
            }
        }
        for method in class.methods() {
            // if !self.contains(method.return_type.as_ref()) {
            //     return Err(Error::ClassNotDefined(return_type.as_ref().clone()));
            // }
        }

        Ok(self.unchecked_insert(class))
    }

    /// Tries to insert a class into this hierarchy.
    ///
    /// If successful, a class ref is returned
    fn unchecked_insert(&mut self, class: Class) -> ClassRef {
        let class_ref = class.get_ref();
        let class_idx = self.graph.add_node(class_ref.clone());

        if let Some(super_class) = class.super_class() {
            let super_class_idx = self
                .get_class_idx(super_class.as_ref())
                .unwrap_or_else(|| panic!("{super_class} is not present within type hierarchy"));
            self.graph.add_edge(
                class_idx,
                super_class_idx,
                Vec::from(super_class.generics()),
            );
        }

        for mixin in class.mixins() {
            let mixin_idx = self
                .get_class_idx(mixin.as_ref())
                .unwrap_or_else(|| panic!("{mixin} is not present within type hierarchy"));
            self.graph
                .add_edge(class_idx, mixin_idx, Vec::from(mixin.generics()));
        }
        self.class_ref_map
            .insert(class_ref.clone(), (class_idx, class));
        class_ref
    }

    /// Tries to get a class from a class ref
    pub fn get(&self, class_ref: &ClassRef) -> Option<&Class> {
        self.class_ref_map.get(class_ref).map(|(_, class)| class)
    }

    /// Tries to get a class from a class ref
    fn get_class_idx(&self, class_ref: &ClassRef) -> Option<NodeIndex> {
        self.class_ref_map
            .get(class_ref)
            .map(|(node_idx, _)| *node_idx)
    }

    /// Checks if this type hierarchy has the given class reference.
    pub fn contains(&self, cls: &ClassRef) -> bool {
        self.class_ref_map.contains_key(cls)
    }

    /// Creates a new [`ClassInst`](ClassInst) value with default generic parameters for this type
    pub fn instantiate_default<C: AsClassRef>(&self, cls_ref: &C) -> Result<ClassInst> {
        let cls_ref = cls_ref.as_class_ref();
        let cls = self
            .get(&cls_ref)
            .ok_or_else(|| Error::ClassNotDefined(cls_ref.clone()))?;

        let instantiated =
            ClassInst::with_generics(cls_ref, cls.generics().iter().map(|s| s.as_invariant()));
        self.add_to_instantiated_set(&instantiated);
        Ok(instantiated)
    }

    /// Creates a new [`ClassInst`](ClassInst) value given a class reference and generics
    pub fn instantiate<C: AsClassRef, I: IntoIterator<Item = GenericParameterBound>>(
        &self,
        cls_ref: &C,
        generics: I,
    ) -> Result<ClassInst> {
        let cls_ref = cls_ref.as_class_ref();
        let cls = self
            .get(&cls_ref)
            .ok_or_else(|| Error::ClassNotDefined(cls_ref.clone()))?;
        let generics = generics.into_iter().collect::<Vec<_>>();
        if cls.generics().len() != generics.len() {
            return Err(Error::WrongNumberOfGenericParameters {
                expected: cls.generics().len(),
                received: generics.len(),
            });
        }

        let validated_generics = cls.generics().iter().zip(generics.into_iter()).try_fold(
            Vec::new(),
            |mut accum: Vec<GenericParameterBound>, (dec, usage)| -> Result<_> {
                let bound = dec.bound();
                let usage_cls = usage.bound_class_instance();
                self.is_assignable(usage_cls, bound)?;
                accum.push(usage);
                Ok(accum)
            },
        )?;

        let instantiated = ClassInst::with_generics(cls_ref, validated_generics);
        self.add_to_instantiated_set(&instantiated);
        Ok(instantiated)
    }

    fn add_to_instantiated_set(&self, instantiated: &ClassInst) {
        let mut all_instantiated = self.instantiated.write();
        all_instantiated.insert(instantiated.clone());
        if let Ok(result) = self.get_all_in_inheritance_tree(instantiated) {
            for parents in result {
                all_instantiated.insert(parents);
            }
        }
    }

    /// Checks if a given class can be assigned to a `target` class.
    ///
    /// Returns `Ok(())` if assignable, and `Err(e)` otherwise
    pub fn is_assignable(&self, class: &ClassInst, target: &ClassInst) -> Result<()> {
        let is_assignable = |class: &ClassInst, target: &ClassInst| -> Result<()> {
            if !self.is_covariant(class, target)? {
                return Err(Error::ClassIsNotCovariant(class.clone(), target.clone()));
            };

            let paths = self.get_inheritance_paths(class, target)?;
            if paths.is_empty() {
                return Err(Error::ClassIsNotAssignable(
                    class.clone(),
                    target.clone(),
                    None,
                ));
            }

            let mut outcomes = vec![];
            for path in &paths {
                let mut ptr = class;
                let mut good = true;
                'path: for inst in path {
                    for (path, ptr) in inst.generics().iter().zip(ptr.generics()) {
                        let ptr_type = ptr.bound_class_instance();
                        print!("checking if {} assignable to {}... ", ptr_type, path);
                        match path {
                            GenericParameterBound::Invariant(i) => {
                                if !self.is_invariant(ptr_type, i)? {
                                    println!(" err");
                                    outcomes.push(Err(Error::ClassIsNotCovariant(
                                        ptr_type.clone(),
                                        i.clone(),
                                    )));
                                    good = false;
                                    break 'path;
                                }
                            }
                            GenericParameterBound::Covariant(i) => {
                                if !self.is_covariant(ptr_type, i)? {
                                    println!(" err");
                                    outcomes.push(Err(Error::ClassIsNotCovariant(
                                        ptr_type.clone(),
                                        i.clone(),
                                    )));
                                    good = false;
                                    break 'path;
                                }
                            }
                            GenericParameterBound::Contravariant(i) => {
                                if !self.is_contravariant(ptr_type, i)? {
                                    println!(" err");
                                    outcomes.push(Err(Error::ClassIsNotContravariant(
                                        ptr_type.clone(),
                                        i.clone(),
                                    )));
                                    good = false;
                                    break 'path;
                                }
                            }
                        }
                        println!("ok")
                    }
                    ptr = inst;
                }
                if good {
                    outcomes.push(Ok(()))
                }
            }

            outcomes.into_iter().try_for_each(|next| next)
        };

        is_assignable(class, target).map_err(|e| {
            Error::ClassIsNotAssignable(class.clone(), target.clone(), Some(Box::new(e)))
        })
    }

    /// checks if a given class is covariant with a parent class
    pub fn is_covariant(&self, class: &ClassInst, parent: &ClassInst) -> Result<bool> {
        let class_idx = self
            .get_class_idx(class.as_ref())
            .ok_or_else(|| Error::ClassNotDefined(class.as_ref().clone()))?;
        let parent_idx = self
            .get_class_idx(parent.as_ref())
            .ok_or_else(|| Error::ClassNotDefined(class.as_ref().clone()))?;

        if class_idx == parent_idx {
            return Ok(true);
        }

        Ok(petgraph::algo::has_path_connecting(
            &self.graph,
            class_idx,
            parent_idx,
            None,
        ))
    }

    /// checks if a given class is invariant with a parent class
    pub fn is_invariant(&self, class: &ClassInst, parent: &ClassInst) -> Result<bool> {
        let class_idx = self
            .get_class_idx(class.as_ref())
            .ok_or_else(|| Error::ClassNotDefined(class.as_ref().clone()))?;
        let parent_idx = self
            .get_class_idx(parent.as_ref())
            .ok_or_else(|| Error::ClassNotDefined(class.as_ref().clone()))?;

        Ok(class_idx == parent_idx)
    }

    /// checks if a given class is covariant with a parent class
    pub fn is_contravariant(&self, class: &ClassInst, child: &ClassInst) -> Result<bool> {
        let class_idx = self
            .get_class_idx(class.as_ref())
            .ok_or_else(|| Error::ClassNotDefined(class.as_ref().clone()))?;
        let child_idx = self
            .get_class_idx(child.as_ref())
            .ok_or_else(|| Error::ClassNotDefined(class.as_ref().clone()))?;

        Ok(petgraph::algo::has_path_connecting(
            &self.graph,
            child_idx,
            class_idx,
            None,
        ))
    }

    pub fn get_inheritance_paths(
        &self,
        class: &ClassInst,
        parent: &ClassInst,
    ) -> Result<Vec<Vec<ClassInst>>> {
        let class_idx = self
            .get_class_idx(class.as_ref())
            .ok_or_else(|| Error::ClassNotDefined(class.as_ref().clone()))?;
        let parent_idx = self
            .get_class_idx(parent.as_ref())
            .ok_or_else(|| Error::ClassNotDefined(class.as_ref().clone()))?;

        let mut paths: Vec<Vec<ClassInst>> =
            petgraph::algo::all_simple_paths(&self.graph, class_idx, parent_idx, 0, None).fold(
                Vec::new(),
                |mut accum, s: VecDeque<NodeIndex>| {
                    let mut path = vec![];
                    for i in 1..s.len() {
                        let from_idx = s[i - 1];
                        let to_idx = s[i];
                        let to_node = self.graph[to_idx].clone();

                        let edge_edx = self.graph.find_edge(from_idx, to_idx).unwrap();
                        let edge = self.graph[edge_edx].clone();

                        let inst = ClassInst::with_generics(to_node, edge);
                        path.push(inst);
                    }
                    accum.push(path);
                    accum
                },
            );
        if class_idx == parent_idx {
            paths.push(vec![parent.clone()])
        }

        Ok(paths)
    }

    pub fn get_all_in_inheritance_tree(&self, class: &ClassInst) -> Result<Vec<ClassInst>> {
        let target = ClassInst::new(self.base_class.clone());
        self.get_inheritance_paths(class, &target)
            .map(|s| s.iter().flatten().cloned().collect())
    }

    /// Gets an iterator over all the classes in this hierarchy in no particular order.
    ///
    /// ```rust
    /// # use aroma_types::hierarchy::ClassHierarchy;
    /// let v = ClassHierarchy::new();
    /// assert!(v.classes().count() > 0, "class hierarchy will always have intrinsic types");
    /// ```
    pub fn classes(&self) -> ClassIterator {
        ClassIterator(Box::new(self.class_ref_map.values().map(|(_, cls)| cls)))
    }

    /// Gets the base class
    pub fn base_class(&self) -> &Class {
        self.get(&self.base_class).unwrap()
    }

    /// Gets a set of all instantiated classes
    pub fn instantiated(&self) -> HashSet<ClassInst> {
        self.instantiated.read().clone()
    }
}

impl Index<&ClassRef> for ClassHierarchy {
    type Output = Class;

    fn index(&self, index: &ClassRef) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl Index<&ClassInst> for ClassHierarchy {
    type Output = Class;

    fn index(&self, index: &ClassInst) -> &Self::Output {
        &self[index.as_ref()]
    }
}

/// An iterator over classes in this hierarchy
pub struct ClassIterator<'a>(Box<dyn Iterator<Item = &'a Class> + 'a>);

impl<'a> Iterator for ClassIterator<'a> {
    type Item = &'a Class;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0} is already defined")]
    ClassAlreadyDefined(ClassRef),
    #[error("{0} is not defined")]
    ClassNotDefined(ClassRef),
    #[error("expected {expected} generic parameters but received {received}")]
    WrongNumberOfGenericParameters { expected: usize, received: usize },
    #[error("{0} is not covariant with {1}")]
    ClassIsNotCovariant(ClassInst, ClassInst),
    #[error("{0} is not contravariant with {1}")]
    ClassIsNotContravariant(ClassInst, ClassInst),
    #[error("{0} is not invariant with {1}")]
    ClassIsNotInvariant(ClassInst, ClassInst),
    #[error("{0} can not be assigned to a value of type {1}{}", .2.as_ref().map(|e| format!(": {e}")).unwrap_or_default()
    )]
    ClassIsNotAssignable(ClassInst, ClassInst, Option<Box<Error>>),
    #[error("{0} does not have a parent class")]
    AllClassesMustHaveParent(ClassRef),
}

pub type Result<T = ()> = std::result::Result<T, Error>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_classes_covariant_with_base_class() {
        let hierarchy = ClassHierarchy::new();
        println!("hierarchy: {:#?}", hierarchy);
        let base_class = &hierarchy
            .instantiate_default(&hierarchy.base_class().get_ref())
            .unwrap();
        for cls in hierarchy.classes() {
            let cls = hierarchy.instantiate_default(&cls.get_ref()).unwrap();
            assert!(
                hierarchy.is_covariant(&cls, base_class).unwrap(),
                "{cls} is not covariant with {base_class}"
            );
        }
    }

    #[test]
    fn test_covariance() {
        let hierarchy = ClassHierarchy::new();
        let inner = hierarchy
            .instantiate(
                &CLASS_CLASS.get_ref(),
                [GenericParameterBound::Covariant(
                    hierarchy
                        .instantiate_default(&OBJECT_CLASS.get_ref())
                        .unwrap(),
                )],
            )
            .unwrap();
        let class_class = hierarchy
            .instantiate(
                &CLASS_CLASS.get_ref(),
                [GenericParameterBound::Invariant(inner)],
            )
            .unwrap();
        println!("class_class: {:?}", class_class);
    }

    #[test]
    fn test_assignability() {
        let hierarchy = ClassHierarchy::new();
        let target = hierarchy
            .instantiate(
                &CLASS_CLASS.get_ref(),
                [GenericParameterBound::Covariant(
                    hierarchy
                        .instantiate_default(&OBJECT_CLASS.get_ref())
                        .unwrap(),
                )],
            )
            .unwrap();
        let inst = hierarchy
            .instantiate(
                &CLASS_CLASS.get_ref(),
                [GenericParameterBound::Invariant(
                    hierarchy.instantiate_default(&I32_CLASS.get_ref()).unwrap(),
                )],
            )
            .unwrap();
        hierarchy
            .is_assignable(&inst, &target)
            .expect("should be assignable");
        let err = hierarchy
            .is_assignable(&target, &inst)
            .expect_err("should not be assignable");
        assert!(matches!(err, Error::ClassIsNotAssignable(..)));
        println!("error: {err}")
    }

    #[test]
    fn test_checked_instantiate() {
        let hierarchy = ClassHierarchy::new();
        let string = hierarchy
            .instantiate(&*STRING_CLASS, [])
            .expect("could not create string ty");
        hierarchy
            .instantiate(
                &*STRING_CLASS,
                [GenericParameterBound::Invariant(string.clone())],
            )
            .expect_err("wrong number of parameters");
        let string_class = hierarchy
            .instantiate(
                &*CLASS_CLASS,
                [GenericParameterBound::Invariant(string.clone())],
            )
            .unwrap();
        let real = &hierarchy[&string_class];
        println!("real: {real:#?}");
    }
}
