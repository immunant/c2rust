use std::mem;
use std::ops::{Deref, DerefMut, Index, IndexMut};
use std::slice;

use util::{Nullable, PrefixPtr, mk_slice, mk_slice_mut};


/// Safe C-like array.  The length is stored in an additional metadata field just before the first
/// array element.
///
/// Unlike `CBlockPtr`, `CArray` maintains ownership of its allocated memory, and deallocates on
/// drop.  Hence it is not `Copy`.  Use `offset`, `CArrayOffset::from_array_ptr`, or the ordinary
/// `Deref` impl to get a non-owning reference to the array contents.
///
/// Safe access requires `T: Nullable`.  This is because `CArray` initializes the array contents to
/// all zeros upon allocation, and there's no guarantee that any particular element will be written
/// before it is read.
#[derive(Debug)]
pub struct CArray<T>(
    /// Internal invariants:
    ///  - The pointer is null when the length is zero.
    ///  - Otherwise, it is non-null.  This applies even to zero-sized types, because we need to
    ///    allocate some space to store the array length.
    PrefixPtr<T, usize>,
);
unsafe impl<T> Nullable for CArray<T> {}

impl<T> CArray<T> {
    /// Construct a pointer to an empty array.
    // It's safe for this to operate even when `T` is not `Nullable`.  Since the array is empty, no
    // `T`s are ever constructed or dropped as a result of this call.
    pub const fn empty() -> CArray<T> {
        CArray(PrefixPtr::null())
    }

    pub fn len(&self) -> usize {
        if self.0.is_null() {
            return 0;
        }
        unsafe { *self.0.metadata() }
    }

    /// Construct a `CArray` from a raw pointer.  The pointer must be the result of a previous
    /// `into_ptr` call (or `as_array_ptr` followed by `mem::forget` on the original `CArray`).
    pub const unsafe fn from_ptr(ptr: *mut T) -> CArray<T> {
        CArray(PrefixPtr::from_ptr(ptr))
    }
}

impl<T: Nullable> CArray<T> {
    /// Allocate a new array of length `len`.
    pub fn alloc(len: usize) -> CArray<T> {
        if len == 0 {
            return Self::empty();
        }

        let ptr = PrefixPtr::<T, usize>::alloc_zeroed(len);
        unsafe {
            *ptr.metadata() = len;
        }
        CArray(ptr)
    }

    /// Free the array.
    ///
    /// This is just a wrapper around `mem::drop`.  It's provided for convenience of migration from
    /// other pointer wrappers, such as `CBlockPtr`.
    pub fn free(self) {
        mem::drop(self);
    }

    /// Get the raw data pointer for this array.  Named `as_array_ptr` to avoid conflict with
    /// `CArrayOffset::as_ptr`, which can return a different value that's not usable with
    /// `CArray::from_ptr` / `CArrayOffset::from_array_ptr`.
    pub fn as_array_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }

    /// Convert this array into a raw pointer.  Does not deallocate - it is the caller's
    /// responsibility to ensure that the array's allocation is eventually freed.
    pub fn into_ptr(self) -> *mut T {
        let p = self.0.as_ptr();
        mem::forget(self);
        p
    }
}

impl<T: Nullable> Deref for CArray<T> {
    type Target = CArrayOffset<T>;
    fn deref(&self) -> &CArrayOffset<T> {
        unsafe { CArrayOffset::from_array_ptr(self.as_array_ptr()) }
    }
}

impl<T: Nullable> DerefMut for CArray<T> {
    fn deref_mut(&mut self) -> &mut CArrayOffset<T> {
        unsafe { CArrayOffset::from_array_ptr_mut(self.as_array_ptr()) }
    }
}

impl<T> Drop for CArray<T> {
    fn drop(&mut self) {
        unsafe {
            if !self.0.is_null() {
                let len = self.len();
                for i in 0 .. len as isize {
                    self.0.offset(i).drop_in_place();
                }
                self.0.free(len);
            }
        }
    }
}


/// The types `&/&mut CArrayOffset<T>` are references into a `CArray<T>`.  These references can be
/// offset forward and backward, supporting typical C pointer arithmetic idioms, and can be safely
/// dereferenced or indexed to access individual array elements.  All accesses are bounds-checked
/// against the underlying array.
#[repr(transparent)]
pub struct CArrayOffset<T> {
    /// DO NOT ACCESS.  Having this field lets `&CArrayOffset` be a fat pointer, but we abuse the
    /// fat pointer's fields to store arbitrary data.  So trying to read or write the elements of
    /// this dummy slice can lead to out-of-bounds memory accesses.
    ///
    ///
    /// # Data representation
    ///
    /// Since `CArrayOffset`'s last field is a slice, the type itself becomes a DST, and references
    /// to it become slice-style fat pointers, which contain a `*mut T` pointer and a `usize`
    /// length.  We abuse this representation to instead store a `PrefixPtr<T, usize>` to the
    /// length-prefixed array data and an `isize` offset of the pointed-to element.  The hope is
    /// that this is close enough to a normal slice representation that we won't run into any
    /// trouble.  (In particular, the compiler won't generate code that dereferences the second
    /// field, as it might if we tried to do this with a trait object instead.)
    _dummy: [T],
}

impl<T: Nullable> CArrayOffset<T> {
    /// Convert a `CArray` data pointer into something that's guaranteed to be non-null.  Null is
    /// mapped to `PrefixPtr::zeroed_meta()`, so its length metadata reads as zero, and we never
    /// access any `T` values.
    ///
    /// We use this function to get the fake base pointer for our dummy slice.
    fn nonnull_prefix_ptr(ptr: *mut T) -> PrefixPtr<T, usize> {
        if ptr.is_null() {
            PrefixPtr::zeroed_meta()
        } else {
            PrefixPtr::from_ptr(ptr)
        }
    }

    /// Construct a reference to the start of a `CArray<T>`, using a pointer returned by
    /// `CArray::as_array_ptr()`.
    pub unsafe fn from_array_ptr<'a>(ptr: *const T) -> &'a CArrayOffset<T> {
        Self::from_raw_parts(ptr, 0)
    }

    pub unsafe fn from_array_ptr_mut<'a>(ptr: *mut T) -> &'a mut CArrayOffset<T> {
        Self::from_raw_parts_mut(ptr, 0)
    }

    /// Construct a reference to an offset into a `CArray<T>` from the base array pointer and an
    /// offset.
    pub unsafe fn from_raw_parts<'a>(ptr: *const T, off: isize) -> &'a CArrayOffset<T> {
        let ptr = Self::nonnull_prefix_ptr(ptr as *mut T);
        mem::transmute(slice::from_raw_parts(ptr.as_ptr(), off as usize))
    }

    pub unsafe fn from_raw_parts_mut<'a>(ptr: *mut T, off: isize) -> &'a mut CArrayOffset<T> {
        let ptr = Self::nonnull_prefix_ptr(ptr);
        mem::transmute(slice::from_raw_parts_mut(ptr.as_ptr(), off as usize))
    }

    /// Get the data pointer of the underlying array.
    fn base(&self) -> PrefixPtr<T, usize> {
        PrefixPtr::from_ptr(self._dummy.as_ptr() as *mut T)
    }

    /// Get the offset of the current element.
    fn off(&self) -> isize {
        self._dummy.len() as isize
    }

    /// Offset the current reference forward or backward, obtaining a new reference into the same
    /// array.
    ///
    /// Constructing references to out-of-range elements is permitted, but dereferencing them while
    /// out-of-bounds will cause a panic.
    pub fn offset(&self, off: isize) -> &CArrayOffset<T> {
        unsafe { Self::from_raw_parts(self.base().as_ptr(), self.off() + off) }
    }

    pub fn offset_mut(&mut self, off: isize) -> &mut CArrayOffset<T> {
        unsafe { Self::from_raw_parts_mut(self.base().as_ptr(), self.off() + off) }
    }

    /// Returns the number of accessible elements starting from the current offset.  If the offset
    /// is negative or past the end, the return value is 0.
    pub fn len(&self) -> usize {
        let base = self.base();
        let off = self.off();

        let base_len = unsafe { *base.metadata() };
        if off < 0 || off as usize >= base_len {
            return 0;
        } else {
            base_len - off as usize
        }
    }

    /// Returns a pointer to the element at the current offset.  Panics if the current offset is
    /// not in bounds or "one past the end".
    pub fn as_ptr(&self) -> *const T {
        let base = self.base();
        let off = self.off();

        unsafe {
            // Reminder: just calling `pointer::offset`, even without dereferencing, is undefined
            // behavior if `base` is invalid or `off` exceeds its bounds.  We panic in those cases.
            //
            // We can't just use `array_offset_len` here because we want a valid result when the offset is
            // "past the end".
            let base_len = *base.metadata();
            assert!(0 <= off && off as usize <= base_len,
                    "offset out of bounds (offset: {}, len: {})", off, base_len);
            base.offset(off)
        }
    }

    /// Returns a pointer to the element at the current offset.  Panics if the current offset is
    /// not in bounds or "one past the end".
    pub fn as_mut_ptr(&mut self) -> *mut T {
        self.as_ptr() as *mut T
    }

    /// Obtain a reference to the current element.
    pub fn as_ref(&self) -> &T {
        &self.as_slice()[0]
    }

    pub fn as_mut(&mut self) -> &mut T {
        &mut self.as_mut_slice()[0]
    }

    /// Obtain a slice reaching from the current element to the end of the underlying array.
    pub fn as_slice(&self) -> &[T] {
        unsafe { mk_slice(self.as_ptr(), self.len()) }
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        unsafe { mk_slice_mut(self.as_mut_ptr(), self.len()) }
    }
}

impl<T: Nullable> Deref for CArrayOffset<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.as_ref()
    }
}

impl<T: Nullable> DerefMut for CArrayOffset<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.as_mut()
    }
}

impl<T: Nullable, I> Index<I> for CArrayOffset<T> where [T]: Index<I> {
    type Output = <[T] as Index<I>>::Output;
    fn index(&self, idx: I) -> &Self::Output {
        self.as_slice().index(idx)
    }
}

impl<T: Nullable, I> IndexMut<I> for CArrayOffset<T> where [T]: IndexMut<I> {
    fn index_mut(&mut self, idx: I) -> &mut Self::Output {
        self.as_mut_slice().index_mut(idx)
    }
}
