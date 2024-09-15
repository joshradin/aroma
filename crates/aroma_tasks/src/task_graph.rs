use crate::task::OwnedTask;
use crate::Task;
use petgraph::algo::toposort;
use petgraph::prelude::*;
use std::collections::{HashMap, HashSet};
use std::convert::Infallible;
use std::marker::PhantomData;
use std::sync::Arc;
use tokio::sync::Mutex;

/// The type of dependency between tasks
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum DependencyType {
    Strong,
    Weak,
}

/// A completed task graph
#[derive(Debug)]
pub struct TaskGraph<S: Send + Sync = (), E: Send + Sync = Infallible> {
    pub(crate) di_graph: StableDiGraph<Arc<Mutex<OwnedTask<S, E>>>, DependencyType>,
}

impl<S: Send + Sync + 'static, E: Send + Sync + 'static> TaskGraph<S, E> {
    /// Creates a task graph builder
    #[inline]
    pub fn builder() -> TaskGraphBuilder<S, E> {
        TaskGraphBuilder::new()
    }
}

/// Used for building task graphs.
///
/// ```rust
/// # use aroma_tasks::{TaskGraph, TaskGraphBuilder};
/// let mut builder: TaskGraphBuilder = TaskGraph::builder();
/// let task_a = builder.add("A", |state| async {Ok(())}).unwrap();
/// let task_b = builder.add("B", |state| async {Ok(())}).unwrap();
/// let task_c = builder.add_then_configure("C", |state| async { Ok(())}, |c| {
///     c.depends_on(&task_a);
/// }).unwrap();
///
/// let built = builder.finish().expect("could not build");
/// ```
#[derive(Debug)]
pub struct TaskGraphBuilder<S: Send + Sync = (), E: Send + Sync = Infallible> {
    tasks: HashMap<String, OwnedTask<S, E>>,
    depends_on: HashMap<String, HashSet<(String, DependencyType)>>,
}

impl<S: Send + Sync + 'static, E: Send + Sync + 'static> TaskGraphBuilder<S, E> {
    /// Creates a new, empty task graph builder
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a task to the builder. By default, tasks have no dependencies
    pub fn add<T>(
        &mut self,
        name: impl AsRef<str>,
        task: T,
    ) -> Result<TaskHandle<S>, TaskAlreadyExists>
    where
        T: Task<S, Err = E> + 'static,
    {
        let name = name.as_ref().to_string();
        if self.tasks.contains_key(&name) {
            return Err(TaskAlreadyExists(name));
        }
        let owned_task = OwnedTask::new(name.clone(), task);
        self.tasks.insert(name.clone(), owned_task);
        self.depends_on.insert(name.clone(), HashSet::new());

        Ok(TaskHandle {
            task: name,
            _state: PhantomData,
        })
    }

    /// Adds a task to the builder. By default, tasks have no dependencies
    pub fn add_then_configure<'a, T, F>(
        &'a mut self,
        name: impl AsRef<str>,
        task: T,
        callback: F,
    ) -> Result<TaskHandle<S>, TaskAlreadyExists>
    where
        T: Task<S, Err = E> + 'static,
        F: FnOnce(&mut TaskConfiguration<'a, S, E>),
    {
        let handle = self.add(name, task)?;
        self.configure(&handle, callback);
        Ok(handle)
    }

    /// Configures a given task, if it already exists
    pub fn configure<'a>(
        &'a mut self,
        name: &impl AsTaskName,
        callback: impl FnOnce(&mut TaskConfiguration<'a, S, E>),
    ) {
        let name = name.as_name().to_string();
        if self.tasks.contains_key(&name) {
            let mut configuration = TaskConfiguration {
                name,
                task_graph_builder: self,
            };
            callback(&mut configuration);
        }
    }

    /// Finishes the task graph
    pub fn finish(self) -> Result<TaskGraph<S, E>, BuildTaskGraphError> {
        let mut graph = StableDiGraph::new();
        let mut task_to_node_idx = HashMap::new();
        let mut node_idx_to_task = HashMap::new();

        for (name, task) in self.tasks {
            let node_idx = graph.add_node(Arc::new(Mutex::new(task)));
            task_to_node_idx.insert(name.clone(), node_idx);
            node_idx_to_task.insert(node_idx, name);
        }

        for (name, dependencies) in self.depends_on {
            let node_a = task_to_node_idx[&name];
            for (dep, dep_type) in dependencies {
                let node_b = *task_to_node_idx
                    .get(&dep)
                    .ok_or_else(|| BuildTaskGraphError::TaskDoesNotExist(dep))?;
                graph.add_edge(node_a, node_b, dep_type);
            }
        }

        if let Err(cycle) = toposort(&graph, None) {
            let start = cycle.node_id();
            let cycle = petgraph::algo::tarjan_scc(&graph)
                .into_iter()
                .find(|nodes| nodes.contains(&start))
                .expect("Cycle not found")
                .into_iter()
                .map(|node| node_idx_to_task[&node].clone())
                .collect::<Vec<_>>();

            return Err(BuildTaskGraphError::GraphCyclic(cycle));
        }

        Ok(TaskGraph { di_graph: graph })
    }
}

impl<S: Send + Sync, E: Send + Sync> Default for TaskGraphBuilder<S, E> {
    fn default() -> Self {
        Self {
            tasks: Default::default(),
            depends_on: Default::default(),
        }
    }
}

/// Used for configuring a task
#[derive(Debug, Clone)]
pub struct TaskHandle<S: Send + Sync> {
    task: String,
    _state: PhantomData<S>,
}

/// Used for configuring a task
#[derive(Debug)]
pub struct TaskConfiguration<'a, S: Send + Sync, E: Send + Sync> {
    name: String,
    task_graph_builder: &'a mut TaskGraphBuilder<S, E>,
}

impl<'a, S: Send + Sync, E: Send + Sync> TaskConfiguration<'a, S, E> {
    /// Adds a depends on relation on to the task. The given task doesnt have to
    /// exist until the task is finished
    pub fn depends_on<T: AsTaskName + ?Sized>(&mut self, name: &T) -> &mut Self {
        let name = name.as_name();
        let set = self
            .task_graph_builder
            .depends_on
            .get_mut(&self.name)
            .expect("set always exists");
        set.insert((name.to_string(), DependencyType::Strong));
        self
    }
}

/// Gets this as a task name
pub trait AsTaskName {
    /// Gets the task name
    fn as_name(&self) -> &str;
}
impl AsTaskName for str {
    fn as_name(&self) -> &str {
        self.as_ref()
    }
}
impl AsTaskName for String {
    fn as_name(&self) -> &str {
        self.as_ref()
    }
}

impl<S: Send + Sync> AsTaskName for TaskHandle<S> {
    fn as_name(&self) -> &str {
        self.task.as_str()
    }
}

/// The given task already exists
#[derive(Debug, thiserror::Error)]
#[error("Task {0:?} already exists")]
pub struct TaskAlreadyExists(String);

/// An error occurred while trying to create this task graph
#[derive(Debug, thiserror::Error)]
pub enum BuildTaskGraphError {
    #[error("A task named {0} does not exist")]
    TaskDoesNotExist(String),
    #[error("Task cycle detected on {0:?}")]
    GraphCyclic(Vec<String>),
}

#[cfg(test)]
mod tests {
    use crate::{BuildTaskGraphError, TaskGraphBuilder};
    use std::collections::HashSet;

    #[test]
    fn test_create_task_graph() {
        let mut graph_builder: TaskGraphBuilder = TaskGraphBuilder::new();
        let a = graph_builder.add("A", |_| async { Ok(()) }).unwrap();
        let b = graph_builder
            .add_then_configure(
                "B",
                |_| async { Ok(()) },
                |b| {
                    b.depends_on(&a);
                },
            )
            .unwrap();
        let c = graph_builder
            .add_then_configure(
                "C",
                |_| async { Ok(()) },
                |c| {
                    c.depends_on(&a);
                },
            )
            .unwrap();
        let d = graph_builder
            .add_then_configure(
                "D",
                |_| async { Ok(()) },
                |d| {
                    d.depends_on(&c);
                    d.depends_on(&b);
                },
            )
            .unwrap();

        let finished = graph_builder.finish().expect("could not finish graph");
        println!("graph: {finished:#?}");
    }

    #[test]
    fn test_create_cyclic_task_graph() {
        let mut graph_builder: TaskGraphBuilder = TaskGraphBuilder::new();
        let a = graph_builder.add("A", |_| async { Ok(()) }).unwrap();
        let b = graph_builder
            .add_then_configure(
                "B",
                |_| async { Ok(()) },
                |b| {
                    b.depends_on(&a);
                },
            )
            .unwrap();
        let c = graph_builder
            .add_then_configure(
                "C",
                |_| async { Ok(()) },
                |c| {
                    c.depends_on(&b);
                },
            )
            .unwrap();
        graph_builder.configure(&a, |a| {
            a.depends_on(&c);
        });

        let BuildTaskGraphError::GraphCyclic(cycle) =
            graph_builder.finish().expect_err("should detect cycle")
        else {
            panic!("cycle expected")
        };
        assert_eq!(
            HashSet::from_iter(cycle),
            HashSet::from(["A".to_string(), "B".to_string(), "C".to_string()])
        )
    }
}
