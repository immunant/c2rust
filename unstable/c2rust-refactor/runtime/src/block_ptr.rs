use std::mem;
use std::ptr::{self, NonNull};
use libc;

use util::Nullable;


/// A pointer to the start of a `malloc`'ed block.  Provides rudimentary bounds checking based on
/// `malloc_usable_size`.  Dereferencing is still unsafe since there is no guarantee that the
/// contents are initialized.
#[derive(Debug)]
pub struct CBlockPtr<T>(
    /// Internal invariants:
    ///  - If `ptr` is null, the array length is zero.
    ///  - If `T` is a zero-sized type, `ptr` is always either null or `NonNull::dangling()`
    ///  - Otherwise, `ptr` is the pointer to the start of a valid `malloc`-provided block
    *mut T,
);

impl<T> Clone for CBlockPtr<T> {
    fn clone(&self) -> CBlockPtr<T> { *self }
}
impl<T> Copy for CBlockPtr<T> {}
unsafe impl<T> Nullable for CBlockPtr<T> {}

impl<T> CBlockPtr<T> {
    pub const fn empty() -> CBlockPtr<T> {
        CBlockPtr(ptr::null_mut())
    }

    pub fn alloc(len: usize) -> CBlockPtr<T> {
        if len == 0 {
            return Self::empty();
        }

        let size = mem::size_of::<T>();
        if size == 0 {
            CBlockPtr(NonNull::dangling().as_ptr())
        } else {
            let byte_len = len.checked_mul(size)
                .unwrap_or_else(|| panic!("overflow when allocating {} elements of size {}",
                                          len, size));
            unsafe {
                let ptr = libc::malloc(byte_len as libc::size_t);
                assert!(!ptr.is_null(), "malloc of {} bytes failed", byte_len);
                CBlockPtr(ptr as *mut T)
            }
        }
    }

    /// Construct a `CBlockPtr` from a raw pointer.  The pointer must point to the start of a
    /// `malloc`'ed block.
    pub const unsafe fn from_ptr(ptr: *mut T) -> CBlockPtr<T> {
        CBlockPtr(ptr)
    }

    pub fn as_ptr(self) -> *mut T {
        self.0
    }

    pub fn into_ptr(self) -> *mut T {
        self.0
    }

    pub unsafe fn free(self) {
        libc::free(self.0 as *mut libc::c_void)
    }

    pub fn offset(self, off: isize) -> CBlockOffset<T> {
        CBlockOffset::new(self, off)
    }

    pub fn offset_mut(self, off: isize) -> CBlockOffset<T> {
        CBlockOffset::new(self, off)
    }

    fn byte_len(self) -> usize {
        if self.0.is_null() {
            return 0;
        }
        unsafe { libc::malloc_usable_size(self.0 as *mut libc::c_void) as usize }
    }

    pub fn len(self) -> usize {
        self.byte_len() / mem::size_of::<T>()
    }

    pub unsafe fn as_ref(&self) -> &'static T {
        assert!(0 < (*self).len(), "out of bounds (index: {}, len: {})",
                0, (*self).len());
        &*self.0
    }

    pub unsafe fn as_mut(&self) -> &'static mut T {
        assert!(0 < self.len(), "out of bounds (index: {}, len: {})",
                0, self.len());
        &mut *self.0
    }
}


/// A pointer to a position within a `malloc`ed block.  Provides rudimentary bounds checking.
/// Dereferencing is still unsafe since there is no guarantee that the contents are initialized.
#[derive(Debug)]
pub struct CBlockOffset<T> {
    base: CBlockPtr<T>,
    off: isize,
}

impl<T> Clone for CBlockOffset<T> {
    fn clone(&self) -> CBlockOffset<T> { *self }
}
impl<T> Copy for CBlockOffset<T> {}
unsafe impl<T> Nullable for CBlockOffset<T> {}

impl<T> CBlockOffset<T> {
    pub fn new(base: CBlockPtr<T>, off: isize) -> CBlockOffset<T> {
        CBlockOffset { base, off }
    }

    pub fn offset(self, off: isize) -> CBlockOffset<T> {
        CBlockOffset {
            base: self.base,
            off: self.off + off,
        }
    }

    pub fn offset_mut(self, off: isize) -> CBlockOffset<T> {
        CBlockOffset {
            base: self.base,
            off: self.off + off,
        }
    }

    pub unsafe fn as_ref(&self) -> &'static T {
        let idx = self.off as usize;
        assert!(idx < self.base.len(), "out of bounds (index: {}, len: {})",
                idx, self.base.len());
        &*self.base.0.offset(self.off)
    }

    pub unsafe fn as_mut(&self) -> &'static mut T {
        let idx = self.off as usize;
        assert!(idx < self.base.len(), "out of bounds (index: {}, len: {})",
                idx, self.base.len());
        &mut *self.base.0.offset(self.off)
    }
}
