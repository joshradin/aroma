use crate::state::State;
use async_trait::async_trait;
use std::fmt::{Debug, Formatter};
use std::future::Future;

/// An asynchronous trait used for representing tasks
#[async_trait]
pub trait Task<S: Send + Sync>: Send + Sync {
    type Err: Send + Sync;

    /// Run this task with access to the state of the executor
    async fn run(&mut self, state: State<S>) -> Result<(), Self::Err>;
}

#[async_trait]
impl<T, Fut, E, S> Task<S> for T
where
    T: Send + Sync,
    S: Send + Sync + 'static,
    E: Send + Sync +'static,
    Fut: Future<Output=Result<(), E>> + Send,
    T: FnMut(State<S>) -> Fut,
{
    type Err = E;

    async fn run(&mut self, state: State<S>)-> Result<(), Self::Err> {
        self(state).await
    }
}

/// Used for holding a named task
pub struct OwnedTask<S: Send + Sync, E: Send + Sync> {
    name: String,
    task: Box<dyn Task<S, Err=E>>,
}

#[async_trait]
impl<S: Send + Sync, E: Send + Sync> Task<S> for OwnedTask<S, E> {
    type Err = E;

    async fn run(&mut self, state: State<S>)-> Result<(), Self::Err> {
        self.task.run(state).await
    }
}

impl<S: Send + Sync + 'static, E: Send + Sync + 'static> OwnedTask<S, E> {
    pub(crate) fn new<T>(name: impl AsRef<str>, task: T) -> Self
        where T: Task<S, Err=E> + 'static
    {
        Self {
            name: name.as_ref().to_string(),
            task: Box::new(task),
        }
    }

    /// Gets the name of the task
    pub fn name(&self) -> &str {
        &self.name
    }
}


impl<S: Send + Sync, E: Send + Sync> Debug for OwnedTask<S, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TaskHandle")
            .field("name", &self.name)
            .finish()
    }
}
