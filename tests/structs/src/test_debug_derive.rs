//! feature_c_variadic

use crate::debug_derive::{rust_kDebuggable1, rust_kDebuggable2};
use std::fmt::Debug;

pub fn test_debuggable() {
    unsafe {
        // Make sure all debuggable structs implement `Debug`
        let _debuggable: Vec<*mut dyn Debug> = vec![rust_kDebuggable1, rust_kDebuggable2];
    }
}
