//! feature_c_variadic

use crate::debug_derive_good::{rust_kS1, rust_kS2};
use std::fmt::Debug;

pub fn test_debuggable() {
    unsafe {
        // Make sure each struct implements `Debug`
        let _debuggable: Vec<*mut dyn Debug> = vec![rust_kS1, rust_kS2];
    }
}
