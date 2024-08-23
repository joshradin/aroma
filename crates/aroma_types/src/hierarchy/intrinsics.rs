//! intrinsic classes

use crate::class::{AsClassRef, Class, ClassInst, ClassKind, ClassRef};
use crate::field::Field;
use crate::generic::GenericDeclaration;
use crate::vis::Vis;
use std::sync::LazyLock;

use crate::method::Method;
use paste::paste;

/// Class class. All classes have a corresponding class object, and this is the
/// class that such objects implement
pub static CLASS_CLASS: LazyLock<Class> = LazyLock::new(class);
/// Base object class
pub static OBJECT_CLASS: LazyLock<Class> = LazyLock::new(object);
pub static ARRAY_CLASS: LazyLock<Class> = LazyLock::new(array);
/// The base class name
pub static BASE_CLASS_NAME: &str = "aroma.system.Object";
/// Class class name
pub static CLASS_CLASS_NAME: &str = "aroma.system.Class";
pub static ARRAY_CLASS_NAME: &str = "aroma.system.Array";

/// Gets the class representation of a given type
pub fn primitive_to_class<T: PrimitiveAsClass>() -> &'static Class {
    T::class()
}

/// Gets the class representation of a given type, determined by a reference to a type
pub fn primitive_ref_to_class<T: PrimitiveAsClass>(_r: &T) -> &'static Class {
    T::class()
}

/// Gets the class representation of a primitive
pub trait PrimitiveAsClass {
    fn class() -> &'static Class;
}
macro_rules! primitive_as_class {
    ($ty:ty => $name:literal) => {
        paste! {
            pub const [<$ty:upper _CLASS_NAME>]: &'static str = $name;

            fn [<$ty:lower _class>]() -> Class {
                Class::new(
                    Vis::Public,
                    ClassKind::Concrete,
                    $name,
                    [],
                    ClassInst::new(OBJECT_CLASS.get_ref()),
                    [],
                    [],
                    []
                    )
            }

            pub static  [<$ty:upper _CLASS>]: LazyLock<Class> = LazyLock::new([<$ty:lower _class>]);

            impl PrimitiveAsClass for $ty {
                fn class() -> &'static Class {
                    &*[<$ty:upper _CLASS>]
                }
            }
        }
    };
}

primitive_as_class!(i32 => "aroma.primitive.Int");
primitive_as_class!(i64 => "aroma.primitive.Long");
primitive_as_class!(String => "aroma.lang.String");

fn class() -> Class {
    Class::new(
        Vis::Public,
        ClassKind::Concrete,
        CLASS_CLASS_NAME,
        [GenericDeclaration::new(
            "T",
            ClassInst::with_generics(ClassRef::from(BASE_CLASS_NAME.to_string()), []),
        )],
        ClassInst::with_generics(ClassRef::from(BASE_CLASS_NAME.to_string()), []),
        [],
        [],
        [Method::new(
            0,
            Vis::Public,
            "getName",
            [],
            ClassInst::new(STRING_CLASS_NAME.to_string().into()),
            [],
            [],
        )],
    )
}

fn object() -> Class {
    Class::new(
        Vis::Public,
        ClassKind::Concrete,
        BASE_CLASS_NAME,
        [],
        None,
        [],
        [
            Field::new_final(Vis::Public, "class", ClassInst::new(CLASS_CLASS.get_ref())),
            Field::new_final(
                Vis::Public,
                "hashcode",
                ClassInst::new(I32_CLASS_NAME.as_class_ref()),
            ),
        ],
        [],
    )
}

fn array() -> Class {
    Class::new(
        Vis::Public,
        ClassKind::Concrete,
        ARRAY_CLASS_NAME,
        [GenericDeclaration::new(
            "T",
            ClassInst::new(OBJECT_CLASS.get_ref()),
        )],
        ClassInst::new(OBJECT_CLASS.get_ref()),
        [],
        [],
        [],
    )
}
