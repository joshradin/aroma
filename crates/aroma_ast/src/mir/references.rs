//! References to other ids along with their type if necessary

use aroma_types::class::ClassInst;
use aroma_types::type_signature::TypeSignature;

/// A name and a type
#[derive(Debug, PartialEq, Hash)]
pub struct NameType(pub String, pub TypeSignature);

/// A field reference
#[derive(Debug, PartialEq, Hash)]
pub struct FieldRef(pub ClassInst, pub NameType);
/// A method reference
#[derive(Debug, PartialEq, Hash)]
pub struct MethodRef(pub ClassInst, pub NameType);
/// An interface method reference
#[derive(Debug, PartialEq, Hash)]
pub struct InterfaceMethodRef(pub ClassInst, pub NameType);
/// A function reference
#[derive(Debug, PartialEq, Hash)]
pub struct FunctionRef(pub NameType);

/// A global variable reference
#[derive(Debug, PartialEq, Hash)]
pub struct GlobalRef(pub NameType);
