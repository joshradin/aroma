#![doc = include_str!("../README.md")]

#[cfg(not(feature = "macros"))]
pub use macros::*;

pub mod debug;
#[cfg(feature = "jit")]
pub mod jit;
mod macros;
pub mod types;
pub mod vm;
pub mod examples;

#[doc(hidden)]
pub mod __export {
    pub use aroma_bytecode::chunk;
}