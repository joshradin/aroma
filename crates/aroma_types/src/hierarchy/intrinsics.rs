//! intrinsic classes

use crate::class::{AsClassRef, Class, ClassInst, ClassKind, ClassRef};
use crate::field::Field;
use crate::generic::GenericDeclaration;
use crate::vis::Vis;
use aroma_tokens::id::Id;
use std::str::FromStr;
use std::sync::LazyLock;

use crate::functions::FunctionDeclaration;

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

macro_rules! primitives {
    ($($ty:ty => $name:literal),+ $(,)?) => {
        mod __primitives {
            use super::*;
            $(
            paste::paste! {
                pub const [<$ty:upper _CLASS_NAME>]: &'static str = $name;

                fn [<$ty:lower _class>]() -> Class {
                    Class::new(
                        Vis::Public,
                        ClassKind::Concrete,
                        Id::from_str($name).unwrap(),
                        [],
                        ClassInst::new(OBJECT_CLASS.get_ref()),
                        [],
                        [],
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
            )*
            pub static PRIMITIVES: &[&LazyLock<Class>] = &[
                $(
                    &paste::paste!([<$ty:upper _CLASS>])
                ),*
            ];
        }

        pub use __primitives::*;
    };
}

/// Void type, used to represent void
#[derive(Debug)]
pub enum Void {}

primitives! {
    i32 => "aroma.primitive.Int",
    i64 => "aroma.primitive.Long",
    f32 => "aroma.primitive.Float",
    f64 => "aroma.primitive.Double",
    bool => "aroma.primitive.Bool",
    u8 => "aroma.primitive.Byte",
    Void => "aroma.primitive.Void",
    String => "aroma.lang.String"
}

fn class() -> Class {
    Class::new(
        Vis::Public,
        ClassKind::Concrete,
        Id::from_str(CLASS_CLASS_NAME).unwrap(),
        [GenericDeclaration::new(
            "T",
            ClassInst::with_generics(ClassRef::from(Id::from_str(BASE_CLASS_NAME).expect("could not parse")), []),
        )],
        ClassInst::with_generics(ClassRef::from(Id::from_str(BASE_CLASS_NAME).expect("could not parse")), []),
        [],
        [],
        [FunctionDeclaration::new(
            Vis::Public,
            "getName",
            [],
            ClassInst::new(ClassRef::from(Id::from_str(STRING_CLASS_NAME).unwrap())),
            [],
            [],
        )],
        [],
        [],
    )
}

fn object() -> Class {
    Class::new(
        Vis::Public,
        ClassKind::Concrete,
        BASE_CLASS_NAME.parse().unwrap(),
        [],
        None,
        [],
        [
            Field::new_final(Vis::Public, "class", ClassInst::new(CLASS_CLASS.get_ref())),
            Field::new_final(
                Vis::Public,
                "hashcode",
                ClassInst::new(Id::from_str(I32_CLASS_NAME).unwrap().into()),
            ),
        ],
        [],
        [],
        [],
    )
}

fn array() -> Class {
    Class::new(
        Vis::Public,
        ClassKind::Concrete,
        ARRAY_CLASS_NAME.parse().unwrap(),
        [GenericDeclaration::new(
            "T",
            ClassInst::new(OBJECT_CLASS.get_ref()),
        )],
        ClassInst::new(OBJECT_CLASS.get_ref()),
        [],
        [],
        [],
        [],
        [],
    )
}
