use std::ops::Deref;
use std::sync::Arc;

/// Contains state that is available to all tasks
#[derive(Debug, Default, PartialEq, Eq, Ord, PartialOrd)]
pub struct State<S: Send + Sync>(Arc<S>);

impl<S: Send + Sync> State<S> {
    /// Creates a new state object
    pub fn new(s: S) -> Self {
        State(Arc::new(s))
    }
}

impl<S: Send + Sync> AsRef<S> for State<S> {
    fn as_ref(&self) -> &S {
        self.0.as_ref()
    }
}

impl<S: Send + Sync> Clone for State<S> {
    fn clone(&self) -> Self {
        Self(Arc::clone(&self.0))
    }
}


impl<S: Send + Sync> Deref for State<S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}