//! feature_c_variadic,

use crate::debug_derive_good::{rust_kS1, rust_get_struct_containing_va_list};

pub fn test_simple_struct() {
    unsafe {
        format!("{rust_kS1:?}");
    }
}

pub fn test_struct_containing_va_list() {
    unsafe {
        let s = rust_get_struct_containing_va_list();
        format!("{s:?}");
    }
}