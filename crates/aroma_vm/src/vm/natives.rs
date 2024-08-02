//! default native functions

use crate::native;
use crate::types::function::ObjNative;

/// print function, emitting to stdout
pub static PRINT: ObjNative = native!(
    fn print(s: Value) {
        println!("{}", s);
        Ok(())
    }
);

/// A slice of all native functions that will be registered into the VM.
pub static NATIVES: &[&'static ObjNative] = &[
    &PRINT,
];
