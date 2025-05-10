use std::ptr::NonNull;
use std::mem;
use std::slice;


pub mod nullable;
pub mod prefix_ptr;

pub use self::nullable::Nullable;
pub use self::prefix_ptr::PrefixPtr;


/// wrapper around `slice::from_raw_parts` that avoids creating null references, even for slices
/// with no backing allocation.
pub unsafe fn mk_slice<'a, T>(ptr: *const T, len: usize) -> &'a [T] {
    let ptr =
        if len == 0 || mem::size_of::<T>() == 0 { NonNull::dangling().as_ptr() }
        else { ptr };
    assert!(!ptr.is_null());
    slice::from_raw_parts(ptr, len)
}

/// wrapper around `slice::from_raw_parts_mut` that avoids creating null references, even for
/// slices with no backing allocation.
pub unsafe fn mk_slice_mut<'a, T>(ptr: *mut T, len: usize) -> &'a mut [T] {
    let ptr =
        if len == 0 || mem::size_of::<T>() == 0 { NonNull::dangling().as_ptr() }
        else { ptr };
    assert!(!ptr.is_null());
    slice::from_raw_parts_mut(ptr, len)
}
