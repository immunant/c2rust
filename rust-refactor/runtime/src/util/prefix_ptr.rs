use std::alloc::{self, Layout};
use std::cmp;
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::ptr;

use util::Nullable;


/// A pointer to zero or more `T`, with the allocated data prefixed by metadata of type `M`.
#[derive(Debug)]
pub struct PrefixPtr<T, M>(*mut T, PhantomData<M>);
impl<T, M> Clone for PrefixPtr<T, M> {
    fn clone(&self) -> PrefixPtr<T, M> { *self }
}
impl<T, M> Copy for PrefixPtr<T, M> {}
unsafe impl<T, M> Nullable for PrefixPtr<T, M> {}

impl<T, M> PrefixPtr<T, M> {
    /// Calculate the alignment of the allocation, which needs to hold elements of both type `T`
    /// and type `M`.
    fn align() -> usize {
        cmp::max(mem::align_of::<T>(), mem::align_of::<M>())
    }

    /// Calculate the length in bytes of the prefix.  This must be large enough to store an `M`,
    /// and must also be a multiple of `T`'s alignment.
    fn prefix_bytes() -> usize {
        let align = Self::align();
        let meta_size = mem::size_of::<M>();
        (meta_size + align - 1) & !align
    }

    /// Calculate the size in bytes of an allocation for `len` `T`s, including the space for the
    /// prefixed `M`.
    fn calc_byte_len(len: usize) -> usize {
        let size = mem::size_of::<T>();

        len.checked_mul(size)
            .and_then(|x| x.checked_add(Self::prefix_bytes()))
            .unwrap_or_else(|| panic!("overflow when allocating {} elements of size {}",
                                      len, size))
    }


    // Allocation functions

    /// Allocate `len` elements of type `T`, prefixed by space for an `M`.  The allocated memory is
    /// all uninitialized.
    pub fn alloc(len: usize) -> PrefixPtr<T, M> {
        unsafe {
            let byte_len = Self::calc_byte_len(len);
            assert!(byte_len > 0, "can't allocate empty PrefixPtr with empty metadata");

            let layout = Layout::from_size_align(byte_len, Self::align()).unwrap();
            let ptr = alloc::alloc(layout);
            if ptr.is_null() {
                alloc::handle_alloc_error(layout);
            }

            let ptr = ptr.offset(Self::prefix_bytes() as isize);
            Self::from_ptr(ptr as *mut T)
        }
    }

    /// Allocate `len` elements of type `T`, prefixed by an `M`.  Both the `T`s and the `M` are
    /// initialized to all zeros.
    pub fn alloc_zeroed(len: usize) -> PrefixPtr<T, M> {
        unsafe {
            let byte_len = Self::calc_byte_len(len);
            assert!(byte_len > 0, "can't allocate empty PrefixPtr with empty metadata");

            let layout = Layout::from_size_align(byte_len, Self::align()).unwrap();
            let ptr = alloc::alloc_zeroed(layout);
            if ptr.is_null() {
                alloc::handle_alloc_error(layout);
            }

            let ptr = ptr.offset(Self::prefix_bytes() as isize);
            Self::from_ptr(ptr as *mut T)
        }
    }

    /// Reallocate the pointed-to memory from `old_len` to `new_len`.  Values of the existing `T`
    /// elements and the prefixed `M` will all be preserved.  If `new_len > old_len`, then the new
    /// `T`s will be uninitialized.
    pub unsafe fn realloc(self, old_len: usize, new_len: usize) -> PrefixPtr<T, M> {
        let old_byte_len = Self::calc_byte_len(old_len);
        let new_byte_len = Self::calc_byte_len(new_len);
        assert!(new_byte_len > 0, "can't allocate empty PrefixPtr with empty metadata");

        let layout = Layout::from_size_align(old_byte_len, Self::align()).unwrap();
        let old_ptr = (self.0 as *mut u8).offset(-(Self::prefix_bytes() as isize));
        let ptr = alloc::realloc(old_ptr, layout, new_byte_len);
        if ptr.is_null() {
            alloc::handle_alloc_error(layout);
        }

        let ptr = ptr.offset(Self::prefix_bytes() as isize);
        Self::from_ptr(ptr as *mut T)
    }

    /// Deallocate the pointed-to memory.  This is unsafe because this must be a valid pointer to
    /// storage allocated with `PrefixPtr`, using the same types `T` and `M`, and `len` must be the
    /// length passed for the original allocation.
    pub unsafe fn free(self, len: usize) {
        let ptr = (self.0 as *mut u8).offset(-(Self::prefix_bytes() as isize));

        let byte_len = Self::calc_byte_len(len);
        let layout = Layout::from_size_align(byte_len, Self::align()).unwrap();
        alloc::dealloc(ptr, layout);
    }


    // Other constructors

    /// Construct a non-null pointer to a static location with no data and all-zero metadata.
    /// There is no need to free the returned pointer.  Since it points to a read-only static
    /// location, it is undefined behavior to modify its metadata.  (Of course, it is also
    /// undefined behavior to modify any of the `T` elements, because none are allocated here.)
    pub fn zeroed_meta() -> PrefixPtr<T, M> {
        static ZEROS: [usize; 4] = [0; 4];
        let base = ZEROS.as_ptr() as *const u8;

        // Compute the offset into `ZEROS` where we'll put the pointer.  It should be properly
        // aligned and have `prefix_bytes` of `ZEROS` before it.  Note that `prefix_bytes` is
        // always a multiple of `Self::align()`, but there's no guarantee that `ZEROS` is aligned
        // to `Self::align()`.
        let offset = Self::prefix_bytes();
        let align_off = base.align_offset(Self::align());
        // align_offset returns usize::MAX when alignment is impossible.
        assert!(align_off < Self::align());
        let offset = offset + align_off;
        assert!(offset <= mem::size_of_val(&ZEROS),
                "not enough space for metadata in preallocated zeros");

        unsafe { Self::from_ptr(base.offset(offset as isize) as *mut T) }
    }

    pub const fn null() -> PrefixPtr<T, M> {
        Self::from_ptr(ptr::null_mut())
    }

    pub const fn from_ptr(ptr: *mut T) -> PrefixPtr<T, M> {
        PrefixPtr(ptr, PhantomData)
    }


    // Accessors.  Note this type also `Deref`s to `*mut T`.

    pub fn as_ptr(self) -> *mut T {
        self.0
    }

    /// Get a pointer to the prefixed metadata.  The result is meaningful only when `self` is a
    /// valid pointer originating from one of the `PrefixPtr` allocation methods (i.e., not just an
    /// arbitrary `*mut T` passed to `from_ptr`).
    pub fn metadata(self) -> *mut M {
        unsafe {
            (self.0 as *mut u8).offset(-(Self::prefix_bytes() as isize)) as *mut M
        }
    }
}

impl<T, M> Deref for PrefixPtr<T, M> {
    type Target = *mut T;
    fn deref(&self) -> &*mut T {
        &self.0
    }
}
