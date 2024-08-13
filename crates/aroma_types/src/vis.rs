/// Visibility of a type
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Vis {
    Public,
    Protected,
    Private,
}

/// Gets the visibility of a type
pub trait Visibility {
    /// gets the visibility of this object
    fn visibility(&self) -> Vis;
    /// Gets a mutable reference to the visibility of this object
    fn visibility_mut(&mut self) -> &mut Vis;
}