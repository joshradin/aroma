#![doc = include_str!("../README.md")]

use cfg_if::cfg_if;

pub mod frontend;
pub mod common;
mod compiler;
pub use compiler::*;



cfg_if! {
    if #[cfg(windows)] {
        #[path = "native/windows.rs"]
        #[doc(hidden)]
        pub mod windows;
        pub use windows as os;
    } else if #[cfg(target_os = "linux")] {
        #[path = "native/linux.rs"]
        #[doc(hidden)]
        pub mod linux;
        pub use linux as os;
    } else if #[cfg(target_os = "macos")] {
        #[path = "native/macos.rs"]
        #[doc(hidden)]
        pub mod macos;
        pub use macos as os;
    } else {
        compile_error!("unsupported OS for compiling")
    }
}