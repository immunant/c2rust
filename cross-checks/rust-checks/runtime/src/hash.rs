
use std::hash::Hasher;
use std::mem;

const MAX_DEPTH: usize = 4;

pub trait XCheckHash<H: Hasher> {
    #[inline]
    fn xcheck_hash(&self) -> u64 {
        self.xcheck_hash_with_depth(MAX_DEPTH)
    }

    fn xcheck_hash_with_depth(&self, depth: usize) -> u64;
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
        impl<H> XCheckHash<H> for $in_ty
                where H: Hasher + Default {
            #[inline]
            fn xcheck_hash_with_depth(&self, _: usize) -> u64 {
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
impl<'a, H, T> XCheckHash<H> for &'a T
        where H: Hasher + Default,
              T: ?Sized + XCheckHash<H> {
    #[inline]
    fn xcheck_hash_with_depth(&self, depth: usize) -> u64 {
        if depth == 0 {
            XCheckHash::<H>::xcheck_hash(&LEAF_REFERENCE_VALUE)
        } else {
            // FIXME: don't decrease the depth when following references?
            (**self).xcheck_hash_with_depth(depth - 1)
        }
    }
}

impl<'a, H, T> XCheckHash<H> for &'a mut T
        where H: Hasher + Default,
              T: ?Sized + XCheckHash<H> {
    #[inline]
    fn xcheck_hash_with_depth(&self, depth: usize) -> u64 {
        if depth == 0 {
            XCheckHash::<H>::xcheck_hash(&LEAF_REFERENCE_VALUE)
        } else {
            // FIXME: don't decrease the depth when following references?
            (**self).xcheck_hash_with_depth(depth - 1)
        }
    }
}

// Hash implementation for raw pointers
impl<H, T> XCheckHash<H> for *const T
        where H: Hasher + Default,
              T: XCheckHash<H> {
    #[inline]
    fn xcheck_hash_with_depth(&self, depth: usize) -> u64 {
        if depth == 0 {
            XCheckHash::<H>::xcheck_hash(&LEAF_POINTER_VALUE)
        } else if self.is_null() {
            XCheckHash::<H>::xcheck_hash(&0usize)
        } else {
            unsafe {
                // FIXME: even non-NULL pointers may be invalid
                (**self).xcheck_hash_with_depth(depth - 1)
            }
        }
    }
}

impl<H, T> XCheckHash<H> for *mut T
        where H: Hasher + Default,
              T: XCheckHash<H> {
    #[inline]
    fn xcheck_hash_with_depth(&self, depth: usize) -> u64 {
        if depth == 0 {
            XCheckHash::<H>::xcheck_hash(&LEAF_POINTER_VALUE)
        } else if self.is_null() {
            XCheckHash::<H>::xcheck_hash(&0usize)
        } else {
            unsafe {
                // FIXME: even non-NULL pointers may be invalid
                (**self).xcheck_hash_with_depth(depth - 1)
            }
        }
    }
}

