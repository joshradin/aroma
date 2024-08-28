//! References to other ids along with their type if necessary

use crate::typed::TypeInfo;
use aroma_types::class::ClassInst;
use aroma_types::type_signature::TypeSignature;

/// A trait for generalizing over types that has [TypeInfo].
pub trait GetInfoTypeRef<T: TypeInfo> {
    /// Gets type
    fn get_info_type_ref(&self) -> T;
}

/// A name and a type
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct NameType(pub String, pub TypeSignature);

impl TypeInfo for NameType {
    fn signature(&self) -> TypeSignature {
        self.1.clone()
    }
}

/// A field reference
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FieldRef(pub ClassInst, pub NameType);
impl TypeInfo for FieldRef {
    fn signature(&self) -> TypeSignature {
        self.1.signature()
    }
}
/// A method reference
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct MethodRef(pub ClassInst, pub NameType);
impl TypeInfo for MethodRef {
    fn signature(&self) -> TypeSignature {
        self.1.signature()
    }
}
/// An interface method reference
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct InterfaceMethodRef(pub ClassInst, pub NameType);
impl TypeInfo for InterfaceMethodRef {
    fn signature(&self) -> TypeSignature {
        self.1.signature()
    }
}
/// A function reference
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FunctionRef(pub NameType);
impl TypeInfo for FunctionRef {
    fn signature(&self) -> TypeSignature {
        self.0.signature()
    }
}

/// A global variable reference
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct GlobalRef(pub NameType);
impl TypeInfo for GlobalRef {
    fn signature(&self) -> TypeSignature {
        self.0.signature()
    }
}
