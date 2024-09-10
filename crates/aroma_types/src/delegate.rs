//! delegate type

use crate::class::ClassInst;

/// delegate type, used as a pseudo interface
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Delegate {
    receiver: Option<ClassInst>,
    args: Vec<ClassInst>,
    return_type: Option<ClassInst>,
}

impl Delegate {
    /// Creates a new delegate type
    pub fn new<Rec, Args, Ret>(delegate: Rec, args: Args, ret: Ret) -> Self
    where
        Rec: Into<Option<ClassInst>>,
        Args: IntoIterator<Item = ClassInst>,
        Ret: Into<Option<ClassInst>>,
    {
        Self {
            receiver: delegate.into(),
            args: Vec::from_iter(args),
            return_type: ret.into(),
        }
    }

    /// Gets the type this function delegates to
    pub fn receiver(&self) -> Option<&ClassInst> {
        self.receiver.as_ref()
    }

    /// Gets the type this function delegates to
    pub fn receiver_mut(&mut self) -> Option<&mut ClassInst> {
        self.receiver.as_mut()
    }

    /// Gets the args for this delegate
    pub fn args(&self) -> &[ClassInst] {
        &self.args
    }

    /// Gets the args for this delegate
    pub fn args_mut(&mut self) -> &mut Vec<ClassInst> {
        &mut self.args
    }

    /// Gets the return type for this delegate
    pub fn returns(&self) -> Option<&ClassInst> {
        self.return_type.as_ref()
    }

    /// Gets the return type for this delegate
    pub fn returns_mut(&mut self) -> Option<&mut ClassInst> {
        self.return_type.as_mut()
    }
}
