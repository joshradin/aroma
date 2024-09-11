//! virtual headers are used to represent what each aroma file declares

use aroma_tokens::id::Id;
use aroma_types::class::{ClassInst, ClassRef};
use aroma_types::generic::GenericDeclaration;
use aroma_types::type_signature::TypeSignature;
use derive_more::From;

/// A virtual header is used to create a collection of all declarations within an aroma translation unit.
#[derive(Debug, Clone, PartialEq)]
pub struct VirtualHeader {
    /// the imported ids used in this virtual header
    imports: Vec<Id>,
    /// the declarations for this header
    declarations: Vec<Declaration>,
}

impl VirtualHeader {
    /// Creates a new virtual header with a given list of declarations and a list
    /// of identifiers that are required for this header.
    ///
    /// Static members are included at the top level of the header.
    pub fn new(
        imports: impl IntoIterator<Item = Id>,
        declarations: impl IntoIterator<Item = Declaration>,
    ) -> Self {
        Self {
            imports: imports.into_iter().collect(),
            declarations: declarations.into_iter().collect(),
        }
    }

    /// Gets a slice of the imports used in this header
    pub fn imports(&self) -> &[Id] {
        &self.imports
    }

    /// Gets a list of the declarations in this header
    pub fn declarations(&self) -> &[Declaration] {
        &self.declarations
    }
}

/// A declaration within a virtual header.
#[derive(Debug, Clone, PartialEq, From)]
pub enum Declaration {
    /// An ID with an actual form, this has a type signature
    IdValue(IdValueDeclaration),
}

/// An ID with an associated type
#[derive(Debug, Clone, PartialEq)]
pub struct IdValueDeclaration {
    id: Id,
    ts: TypeSignature,
}

impl IdValueDeclaration {
    /// Creates a new declaration
    pub fn new(id: Id, ts: TypeSignature) -> Self {
        Self { id, ts }
    }
    /// Gets the id of the declaration
    pub fn id(&self) -> &Id {
        &self.id
    }

    /// Gets the type signature of this value
    pub fn type_signature(&self) -> &TypeSignature {
        &self.ts
    }
}

/// A class declaration.
#[derive(Debug, Clone, PartialEq)]
struct ClassDeclaration {
    id: Id,
    generic_declarations: Vec<GenericDeclaration>,
    class_declarations: Vec<ClassMemberDeclaration>,
}

impl ClassDeclaration {
    /// Creates a new class declaration with a given id and it's declarations
    pub fn new(
        id: Id,
        generic_declarations: impl IntoIterator<Item = GenericDeclaration>,
        class_declarations: impl IntoIterator<Item = ClassMemberDeclaration>,
    ) -> Self {
        Self {
            id,
            generic_declarations: generic_declarations.into_iter().collect(),
            class_declarations: class_declarations.into_iter().collect(),
        }
    }

    /// The id of this class
    pub fn id(&self) -> &Id {
        &self.id
    }

    /// The generic declarations for this class
    pub fn generic_declarations(&self) -> &[GenericDeclaration] {
        &self.generic_declarations
    }

    /// The class members
    pub fn class_declarations(&self) -> &Vec<ClassMemberDeclaration> {
        &self.class_declarations
    }
    /// Gets a [ClassRef] representation for this class
    #[inline]
    pub fn as_class_ref(&self) -> ClassRef {
        ClassRef::from(self.id.clone())
    }
    /// Gets a [ClassInst] representation for this class
    #[inline]
    pub fn as_class_inst(&self) -> ClassInst {
        ClassInst::with_generics(
            self.as_class_ref(),
            self.generic_declarations()
                .iter()
                .map(|gen| gen.as_invariant()),
        )
    }
}

/// A member within a class declaration
#[derive(Debug, Clone, PartialEq, From)]
pub enum ClassMemberDeclaration {
    IdValue(IdValueDeclaration),
    Constructor(ConstructorDeclaration),
    /// A non-static subclass
    ClassDeclaration(ClassDeclaration),
}

/// A constructor declaration, describing the function signature of the constructor.
#[derive(Debug, Clone, PartialEq, From)]
pub struct ConstructorDeclaration(TypeSignature);

impl ConstructorDeclaration {
    /// Creates a new constructor declaration with a given type signature
    pub fn new(ts: TypeSignature) -> Self {
        Self(ts)
    }

    /// Gets the type signature of
    pub fn type_signature(&self) -> &TypeSignature {
        &self.0
    }
}

/// A simple function declaration
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    id: Id,
    generic_declaration: Vec<GenericDeclaration>,
    receiver: Option<TypeSignature>,
    parameters: Vec<TypeSignature>,
    return_type: TypeSignature,
    throws: Vec<TypeSignature>,
}

impl FunctionDeclaration {
    /// Creates a new function declaration
    pub fn new(
        id: Id,
        generic_declaration: impl IntoIterator<Item = GenericDeclaration>,
        receiver: impl Into<Option<TypeSignature>>,
        parameters: impl IntoIterator<Item = TypeSignature>,
        return_type: impl Into<Option<TypeSignature>>,
        throws: impl IntoIterator<Item = TypeSignature>,
    ) -> Self {
        Self {
            id,
            generic_declaration: generic_declaration.into_iter().collect(),
            receiver: receiver.into(),
            parameters: parameters.into_iter().collect(),
            return_type: return_type.into().unwrap_or_default(),
            throws: throws.into_iter().collect(),
        }
    }

    /// Gets the fully qualified name of the function declaration
    pub fn id(&self) -> &Id {
        &self.id
    }

    /// Gets the generic parameters for this function
    pub fn generic_declaration(&self) -> &[GenericDeclaration] {
        &self.generic_declaration
    }

    /// Gets the optional receiver type for this function
    pub fn receiver(&self) -> Option<&TypeSignature> {
        self.receiver.as_ref()
    }

    /// Gets the types of the parameters for this function
    pub fn parameters(&self) -> &[TypeSignature] {
        &self.parameters
    }

    /// The return type for this function
    pub fn return_type(&self) -> &TypeSignature {
        &self.return_type
    }

    /// The list of possible throwable types for this function
    pub fn throws(&self) -> &[TypeSignature] {
        &self.throws
    }
}
