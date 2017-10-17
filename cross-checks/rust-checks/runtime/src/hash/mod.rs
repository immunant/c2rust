
use std::hash::Hasher;
use std::mem;

pub mod djb2;
pub mod simple;
pub mod jodyhash;

const MAX_DEPTH: usize = 4;

// Trait alias for Hasher + Default
pub trait XCheckHasher: Hasher + Default {
    fn write_bool(&mut self, i: bool) {
        self.write_u8(i as u8);
    }

    fn write_char(&mut self, i: char) {
        self.write_u32(i as u32);
    }

    fn write_f32(&mut self, i: f32) {
        self.write_u32(unsafe { mem::transmute(i) });
    }

    fn write_f64(&mut self, i: f64) {
        self.write_u64(unsafe { mem::transmute(i) });
    }
}

// Trait for our cross-check hash function
// The hash function itself takes 2 generic parameters:
//   HA = the hasher for aggregate types, e.g., structs/enums
//   HS = the (fast) hasher to use for simple types, e.g., u32
pub trait XCheckHash {
    #[inline]
    fn xcheck_hash<HA, HS>(&self) -> u64
            where HA: XCheckHasher, HS: XCheckHasher {
        self.xcheck_hash_with_depth::<HA, HS>(MAX_DEPTH)
    }

    fn xcheck_hash_with_depth<HA, HS>(&self, depth: usize) -> u64
            where HA: XCheckHasher, HS: XCheckHasher;
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
            fn xcheck_hash_with_depth<HA, HS>(&self, _: usize) -> u64
                    where HA: XCheckHasher, HS: XCheckHasher {
                // FIXME: this is pretty slow, but has the advantage that
                // the size of the value is rolled into the hash, which
                // roughly approximates rolling the type into the hash
                // What we really want is a fast but good hash function that looks like:
                //   H(type, value: u64) -> u64
                let mut h = HS::default();
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
impl_primitive_hash!(bool,  write_bool);
impl_primitive_hash!(char,  write_char);
impl_primitive_hash!(f32,   write_f32);
impl_primitive_hash!(f64,   write_f64);

// TODO: hash for strings (str type)
// TODO: hash for slices ([T] type)

// Placeholder values for reference/pointers to use when
// we reach depth == 0 and cannot descend any further
const LEAF_REFERENCE_VALUE: u32 = 0xDEADBEEFu32;
const LEAF_POINTER_VALUE: u32 = 0xDEADBEEFu32;

// Hash implementation for references
impl<'a, T: ?Sized + XCheckHash> XCheckHash for &'a T {
    #[inline]
    fn xcheck_hash_with_depth<HA, HS>(&self, depth: usize) -> u64
            where HA: XCheckHasher, HS: XCheckHasher {
        if depth == 0 {
            XCheckHash::xcheck_hash::<HA, HS>(&LEAF_REFERENCE_VALUE)
        } else {
            // FIXME: don't decrease the depth when following references?
            (**self).xcheck_hash_with_depth::<HA, HS>(depth - 1)
        }
    }
}

impl<'a, T: ?Sized + XCheckHash> XCheckHash for &'a mut T {
    #[inline]
    fn xcheck_hash_with_depth<HA, HS>(&self, depth: usize) -> u64
            where HA: XCheckHasher, HS: XCheckHasher {
        if depth == 0 {
            XCheckHash::xcheck_hash::<HA, HS>(&LEAF_REFERENCE_VALUE)
        } else {
            // FIXME: don't decrease the depth when following references?
            (**self).xcheck_hash_with_depth::<HA, HS>(depth - 1)
        }
    }
}

// Hash implementation for raw pointers
impl<T: XCheckHash> XCheckHash for *const T {
    #[inline]
    fn xcheck_hash_with_depth<HA, HS>(&self, depth: usize) -> u64
            where HA: XCheckHasher, HS: XCheckHasher {
        if depth == 0 {
            XCheckHash::xcheck_hash::<HA, HS>(&LEAF_POINTER_VALUE)
        } else if self.is_null() {
            XCheckHash::xcheck_hash::<HA, HS>(&0usize)
        } else {
            unsafe {
                // FIXME: even non-NULL pointers may be invalid
                (**self).xcheck_hash_with_depth::<HA, HS>(depth - 1)
            }
        }
    }
}

impl<T: XCheckHash> XCheckHash for *mut T {
    #[inline]
    fn xcheck_hash_with_depth<HA, HS>(&self, depth: usize) -> u64
            where HA: XCheckHasher, HS: XCheckHasher {
        if depth == 0 {
            XCheckHash::xcheck_hash::<HA, HS>(&LEAF_POINTER_VALUE)
        } else if self.is_null() {
            XCheckHash::xcheck_hash::<HA, HS>(&0usize)
        } else {
            unsafe {
                // FIXME: even non-NULL pointers may be invalid
                (**self).xcheck_hash_with_depth::<HA, HS>(depth - 1)
            }
        }
    }
}

