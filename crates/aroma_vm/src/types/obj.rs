use aroma_types::class::ClassInst;
use aroma_types::hierarchy::Hierarchy;
use std::collections::HashMap;
use std::ptr::NonNull;
use thiserror::Error;

pub struct Object {
    signature: ClassInst,
    layout: NonNull<ObjectLayout>,
    data: Box<[u8]>,
}

#[derive(Debug)]
pub struct ObjectLayout {
    size: usize,
    field_offsets: HashMap<(ClassInst, Option<String>), usize>,
}

impl ObjectLayout {
    /// Creates a new object layout from a class instance and it's hierarchy
    pub fn new(
        class_inst: &ClassInst,
        class_hierarchy: &Hierarchy,
    ) -> Result<ObjectLayout, ObjectLayoutError> {
        let class = class_hierarchy.get(class_inst.as_ref()).ok_or_else(|| {
            aroma_types::hierarchy::Error::ClassNotDefined(class_inst.as_ref().clone())
        })?;

        let mut base = if let Some(super_class) = class.super_class() {
            ObjectLayout::new(super_class, class_hierarchy)?
        } else {
            ObjectLayout {
                size: size_of::<usize>(),
                field_offsets: Default::default(),
            }
        };

        let mut offset = base.size;
        base.field_offsets
            .insert((class_inst.clone(), None), offset);
        offset += size_of::<usize>();

        for field in class.fields() {
            let field_size = match field.type_signature() {

                _ => size_of::<usize>(),
            };

            let padding = (field_size - (offset % field_size)) % field_size;
            let aligned = padding + offset;
            base.field_offsets.insert(
                (class_inst.clone(), Some(field.name().to_string())),
                aligned,
            );

            offset += field_size + padding;
        }
        for mixin in class.mixins() {
            let field_size = size_of::<usize>();
            let padding = (field_size - (offset % field_size)) % field_size;
            let aligned = padding + offset;
            base.field_offsets
                .entry((mixin.clone(), None))
                .or_insert_with(|| {
                    offset += field_size + padding;
                    aligned
                });
        }

        base.size = offset;

        Ok(base)
    }

    ///
    pub fn vtable_offset(&self, class_inst: &ClassInst) -> Option<usize> {
        self.field_offsets.get(&(class_inst.clone(), None)).copied()
    }
}

#[derive(Debug, Error)]
pub enum ObjectLayoutError {
    #[error(transparent)]
    ClassHierarchyError(#[from] aroma_types::hierarchy::Error),
}

#[cfg(test)]
mod tests {
    use crate::types::obj::ObjectLayout;
    use aroma_types::hierarchy::intrinsics::CLASS_CLASS;
    use aroma_types::hierarchy::Hierarchy;

    #[test]
    fn create_base_object_layout() {
        let hierarchy = Hierarchy::new();
        let inst = hierarchy
            .instantiate_default(hierarchy.base_class())
            .expect("could not instantiate");

        let layout = ObjectLayout::new(&inst, &hierarchy).expect("could not create layout");
        println!("layout: {layout:#?}");
    }

    #[test]
    fn create_class_object_layout() {
        let hierarchy = Hierarchy::new();
        let inst = hierarchy
            .instantiate_default(&*CLASS_CLASS)
            .expect("could not instantiate");

        let layout = ObjectLayout::new(&inst, &hierarchy).expect("could not create layout");
        println!("layout: {layout:#?}");
    }
}
