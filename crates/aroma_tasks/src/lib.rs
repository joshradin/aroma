#![doc = include_str!("../README.md")]
//! # Example
//!
//! ```rust
//! use aroma_tasks::{TaskExecutor, TaskGraph, TaskGraphBuilder};
//!
//! # tokio_test::block_on(async {
//! let mut builder: TaskGraphBuilder = TaskGraph::builder();
//! let a = builder.add("a", |_| async { Ok(()) }).unwrap();
//! let b = builder.add("b", |_| async { Ok(()) }).unwrap();
//! builder.add_then_configure("c", |_| async { Ok(())}, |c| {c.depends_on(&a).depends_on(&b);}).unwrap();
//!
//! let mut executor: TaskExecutor = builder.finish().expect("could not finish").into();
//! executor.execute().await.expect("could not finish");
//! # })
//! ```

mod state;
mod task;
mod task_executor;
mod task_graph;

pub use self::{
    state::State,
    task::Task,
    task_executor::TaskExecutor,
    task_graph::*
};
