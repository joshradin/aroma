#![doc = include_str!("../README.md")]

#[cfg(not(feature = "macros"))]
pub use macros::*;

pub mod debug;
pub mod examples;
#[cfg(feature = "jit")]
pub mod jit;
mod macros;
pub mod types;
pub mod vm;

#[doc(hidden)]
pub mod __export {
    pub use aroma_bytecode::chunk;
}
