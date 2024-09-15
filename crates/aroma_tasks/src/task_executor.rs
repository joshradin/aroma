use crate::state::State;
use crate::task::OwnedTask;
use crate::task_graph::TaskGraph;
use crate::Task;
use petgraph::prelude::NodeIndex;
use petgraph::visit::NodeCount;
use petgraph::Direction;
use std::collections::{HashMap, HashSet};
use std::convert::Infallible;
use std::error::Error;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::task::{AbortHandle, JoinSet};
use tracing::{instrument, trace};

/// The task executor
pub struct TaskExecutor<S: Send + Sync = (), E: Send + Sync = Infallible> {
    graph: TaskGraph<S, E>,
    state: State<S>,
    executed: HashSet<String>,
}

impl<S: Send + Sync + 'static,  E: Send + Sync + Error + 'static> TaskExecutor<S, E> {
    /// Creates a new executor with the given initial state
    pub fn with_state(graph: TaskGraph<S, E>, state: S) -> Self {
        Self {
            graph,
            state: State::new(state),
            executed: Default::default(),
        }
    }

    fn get_next_task(
        &mut self,
        running: HashSet<&NodeIndex>,
    ) -> Option<(Arc<Mutex<OwnedTask<S, E>>>, NodeIndex)> {
        self.graph
            .di_graph
            .node_indices()
            .filter(|idx| !running.contains(idx))
            .filter(|idx| {
                let neighbors = self.graph
                                    .di_graph
                                    .neighbors_directed(*idx, Direction::Outgoing);
                neighbors.count() == 0
            })
            .next()
            .and_then(|idx| {
                self.graph
                    .di_graph
                    .node_weight(idx)
                    .map(|task| (task.clone(), idx))
            })
    }

    fn completed(&mut self, task: NodeIndex) {
        self.graph.di_graph.remove_node(task);
    }



    /// Executes all tasks
    #[instrument(skip(self), level = "trace")]
    pub async fn execute(&mut self) -> Result<(), TaskExecutionError<E>> {
        let mut join_set = JoinSet::<Result<NodeIndex, E>>::new();
        let mut aborts = HashMap::<NodeIndex, AbortHandle>::new();
        let mut err = None;
        while self.graph.di_graph.node_count() > 0 {
            while let Some((task, node_idx)) = self.get_next_task(HashSet::from_iter(aborts.keys()))
            {
                let state = self.state.clone();
                trace!("starting task {}", task.lock().await.name());
                aborts.insert(
                    node_idx,
                    join_set.spawn(async move {
                        let mut task = task.lock().await;
                        task.run(state).await?;
                        Ok(node_idx)
                    }),
                );
            }

            if let Some(completed) = join_set.join_next().await {
                let completed_task = completed.map_err(TaskExecutionError::JoinError)??;
                trace!("got finished task {}", self.graph.di_graph[completed_task].lock().await.name());
                let _ = aborts.remove(&completed_task);
                self.completed(completed_task);
            } else {
                if self.graph.di_graph.node_count() > 0 {
                    err = Some(TaskExecutionError::TaskSetEmpty);
                    break;
                } else {
                    break;
                }
            }
        }
        match err {
            Some(err) => {
                for (_, handle) in aborts {
                    handle.abort();
                }
                Err(err)
            }
            None => Ok(()),
        }
    }

    /// Gets the state
    pub fn state(&self) -> &State<S> {
        &self.state
    }
}

impl<S: Send + Sync + Default + 'static,  E: Send + Sync + Error + 'static> TaskExecutor<S, E> {
    /// Creates a new executor with default state
    pub fn new(graph: TaskGraph<S, E>) -> Self {
        Self::with_state(graph, S::default())
    }
}

impl<S: Send + Sync + Default + 'static,  E: Send + Sync + Error + 'static> From<TaskGraph<S, E>> for TaskExecutor<S, E> {
    fn from(value: TaskGraph<S, E>) -> Self {
        Self::new(value)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TaskExecutionError<E: Send + Sync + Error + 'static> {
    #[error("Task set empty but tasks could be ran")]
    TaskSetEmpty,
    #[error(transparent)]
    JoinError(tokio::task::JoinError),
    #[error(transparent)]
    TaskError(#[from] E)
}

#[cfg(test)]
mod tests {
    use std::convert::Infallible;
    use crate::{State, TaskExecutor, TaskGraph, TaskGraphBuilder};
    use std::sync::atomic::{AtomicI32, Ordering};
    use crate::task_executor::TaskExecutionError;

    fn create_graph() -> TaskGraph<AtomicI32> {
        let mut graph_builder: TaskGraphBuilder<AtomicI32> = TaskGraph::builder();
        let add_one = |state: State<AtomicI32>| async move {
            state.fetch_add(1, Ordering::SeqCst);
            Ok::<(), Infallible>(())
        };
        let a = graph_builder.add("A", add_one).unwrap();
        let b = graph_builder
            .add_then_configure("B", add_one, |b| {
                b.depends_on(&a);
            })
            .unwrap();
        let c = graph_builder
            .add_then_configure("C", add_one, |c| {
                c.depends_on(&a);
            })
            .unwrap();
        let d = graph_builder
            .add_then_configure("D", add_one, |d| {
                d.depends_on(&c);
                d.depends_on(&b);
            })
            .unwrap();
        graph_builder.finish().expect("could not finish")
    }

    #[derive(Debug, thiserror::Error)]
    #[error("Error")]
    struct Error;

    fn create_fallible_graph() -> TaskGraph<AtomicI32, Error> {
        let mut graph_builder: TaskGraphBuilder<AtomicI32, _> = TaskGraph::<_, _>::builder();
        let add_one = |state: State<AtomicI32>| async move {
            let s = state.fetch_add(1, Ordering::SeqCst);
            if s == 1 {
                return Err(Error);
            }
            Ok(())
        };
        let a = graph_builder.add("A", add_one).unwrap();
        let b = graph_builder
            .add_then_configure("B", add_one, |b| {
                b.depends_on(&a);
            })
            .unwrap();
        let c = graph_builder
            .add_then_configure("C", add_one, |c| {
                c.depends_on(&a);
            })
            .unwrap();
        let d = graph_builder
            .add_then_configure("D", add_one, |d| {
                d.depends_on(&c);
                d.depends_on(&b);
            })
            .unwrap();
        graph_builder.finish().expect("could not finish")
    }

    #[test_log::test(tokio::test)]
    async fn test_execute() {
        let graph = create_graph();
        let mut executor = TaskExecutor::new(graph);
        executor
            .execute()
            .await
            .expect("could not finish");
        let state = executor.state().load(Ordering::SeqCst);
        assert_eq!(state, 4);
    }

    #[test_log::test(tokio::test)]
    async fn test_execute_fallible() {
        let graph = create_fallible_graph();
        let mut executor = TaskExecutor::new(graph);
        let err = executor
            .execute()
            .await
            .expect_err("should've had an error");
        assert!(matches!(err, TaskExecutionError::TaskError(Error)));
        let state = executor.state().load(Ordering::SeqCst);
        assert_eq!(state, 3);
    }
}
