
use std::hash::Hasher;
use std::mem;

const MAX_DEPTH: usize = 4;

// Trait alias for Hasher + Default
pub trait XCheckHasher: Hasher + Default {}

// Implement XCheckHasher for all types that satisfy the sub-traits
impl<H: Hasher + Default> XCheckHasher for H {}

pub trait XCheckHash {
    #[inline]
    fn xcheck_hash<H: XCheckHasher>(&self) -> u64 {
        self.xcheck_hash_with_depth::<H>(MAX_DEPTH)
    }

    fn xcheck_hash_with_depth<H: XCheckHasher>(&self, depth: usize) -> u64;
}

// Macro that emits xcheck_hash for a given primitive type, hashing
// the value by just calling one of the write_XXX functions in Hasher
macro_rules! impl_primitive_hash {
    ($in_ty:ident, $write_meth:ident) => {
        impl_primitive_hash!($in_ty, $write_meth, |x| x);
    };
    // The third argument is a filter lambda that gets applied
    // to the argument of $write_meth just before the call
    ($in_ty:ident, $write_meth:ident, $val_filter:expr) => {
        impl XCheckHash for $in_ty {
            #[inline]
            fn xcheck_hash_with_depth<H: XCheckHasher>(&self, _: usize) -> u64 {
                // FIXME: this is pretty slow, but has the advantage that
                // the size of the value is rolled into the hash, which
                // roughly approximates rolling the type into the hash
                // What we really want is a fast but good hash function that looks like:
                //   H(type, value: u64) -> u64
                let mut h = H::default();
                h.$write_meth($val_filter(*self));
                h.finish()
            }
        }
    };
}

// Implement XCheckHash for all the integer types
// TODO: would be nice to distinguish between different but same-sized types,
// e.g. between usize and isize
impl_primitive_hash!(u8,    write_u8);
impl_primitive_hash!(u16,   write_u16);
impl_primitive_hash!(u32,   write_u32);
impl_primitive_hash!(u64,   write_u64);
impl_primitive_hash!(usize, write_usize);
impl_primitive_hash!(i8,    write_i8);
impl_primitive_hash!(i16,   write_i16);
impl_primitive_hash!(i32,   write_i32);
impl_primitive_hash!(i64,   write_i64);
impl_primitive_hash!(isize, write_isize);
impl_primitive_hash!(bool,  write_u8,  |x| x as u8);
impl_primitive_hash!(char,  write_u32, |x| x as u32);
impl_primitive_hash!(f32,   write_u32, |x| unsafe { mem::transmute(x) });
impl_primitive_hash!(f64,   write_u64, |x| unsafe { mem::transmute(x) });

// TODO: hash for strings (str type)
// TODO: hash for slices ([T] type)

// Placeholder values for reference/pointers to use when
// we reach depth == 0 and cannot descend any further
const LEAF_REFERENCE_VALUE: u32 = 0xDEADBEEFu32;
const LEAF_POINTER_VALUE: u32 = 0xDEADBEEFu32;

// Hash implementation for references
impl<'a, T: ?Sized + XCheckHash> XCheckHash for &'a T {
    #[inline]
    fn xcheck_hash_with_depth<H: XCheckHasher>(&self, depth: usize) -> u64 {
        if depth == 0 {
            XCheckHash::xcheck_hash::<H>(&LEAF_REFERENCE_VALUE)
        } else {
            // FIXME: don't decrease the depth when following references?
            (**self).xcheck_hash_with_depth::<H>(depth - 1)
        }
    }
}

impl<'a, T: ?Sized + XCheckHash> XCheckHash for &'a mut T {
    #[inline]
    fn xcheck_hash_with_depth<H: XCheckHasher>(&self, depth: usize) -> u64 {
        if depth == 0 {
            XCheckHash::xcheck_hash::<H>(&LEAF_REFERENCE_VALUE)
        } else {
            // FIXME: don't decrease the depth when following references?
            (**self).xcheck_hash_with_depth::<H>(depth - 1)
        }
    }
}

// Hash implementation for raw pointers
impl<T: XCheckHash> XCheckHash for *const T {
    #[inline]
    fn xcheck_hash_with_depth<H: XCheckHasher>(&self, depth: usize) -> u64 {
        if depth == 0 {
            XCheckHash::xcheck_hash::<H>(&LEAF_POINTER_VALUE)
        } else if self.is_null() {
            XCheckHash::xcheck_hash::<H>(&0usize)
        } else {
            unsafe {
                // FIXME: even non-NULL pointers may be invalid
                (**self).xcheck_hash_with_depth::<H>(depth - 1)
            }
        }
    }
}

impl<T: XCheckHash> XCheckHash for *mut T {
    #[inline]
    fn xcheck_hash_with_depth<H: XCheckHasher>(&self, depth: usize) -> u64 {
        if depth == 0 {
            XCheckHash::xcheck_hash::<H>(&LEAF_POINTER_VALUE)
        } else if self.is_null() {
            XCheckHash::xcheck_hash::<H>(&0usize)
        } else {
            unsafe {
                // FIXME: even non-NULL pointers may be invalid
                (**self).xcheck_hash_with_depth::<H>(depth - 1)
            }
        }
    }
}

