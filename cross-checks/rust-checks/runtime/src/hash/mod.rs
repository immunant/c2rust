
use std::hash::Hasher;
use std::mem;

#[cfg(feature="libc-hash")]
use libc;

pub mod djb2;
pub mod simple;
pub mod jodyhash;

const MAX_DEPTH: usize = 8;

// Trait alias for Hasher + Default
pub trait CrossCheckHasher: Hasher + Default {
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
pub trait CrossCheckHash {
    #[inline]
    fn cross_check_hash<HA, HS>(&self) -> Option<u64>
            where HA: CrossCheckHasher, HS: CrossCheckHasher {
        Some(self.cross_check_hash_depth::<HA, HS>(MAX_DEPTH))
    }

    fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
            where HA: CrossCheckHasher, HS: CrossCheckHasher;
}

impl CrossCheckHash for ! {
    #[inline]
    fn cross_check_hash<HA, HS>(&self) -> Option<u64>
            where HA: CrossCheckHasher, HS: CrossCheckHasher {
        panic!("Attempted to CrossCheckHash a 'never' value")
    }

    #[inline]
    fn cross_check_hash_depth<HA, HS>(&self, _depth: usize) -> u64
            where HA: CrossCheckHasher, HS: CrossCheckHasher {
        panic!("Attempted to CrossCheckHash a 'never' value")
    }
}

impl CrossCheckHash for () {
    #[inline]
    fn cross_check_hash<HA, HS>(&self) -> Option<u64>
            where HA: CrossCheckHasher, HS: CrossCheckHasher {
        None
    }

    #[inline]
    fn cross_check_hash_depth<HA, HS>(&self, _depth: usize) -> u64
            where HA: CrossCheckHasher, HS: CrossCheckHasher {
        panic!("Attempted to CrossCheckHash a unit value")
    }
}


// Macro that emits cross_check_hash for a given primitive type, hashing
// the value by just calling one of the write_XXX functions in Hasher
macro_rules! impl_primitive_hash {
    ($in_ty:ident, $write_meth:ident) => {
        impl_primitive_hash!($in_ty, $write_meth, |x| x);
    };
    // The third argument is a filter lambda that gets applied
    // to the argument of $write_meth just before the call
    ($in_ty:ident, $write_meth:ident, $val_filter:expr) => {
        impl CrossCheckHash for $in_ty {
            #[inline]
            fn cross_check_hash_depth<HA, HS>(&self, _: usize) -> u64
                    where HA: CrossCheckHasher, HS: CrossCheckHasher {
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

// Implement CrossCheckHash for all the integer types
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

// Placeholder values for reference/pointers to use when
// we reach depth == 0 and cannot descend any further
const LEAF_REFERENCE_VALUE: u32 = 0xDEADBEEFu32;
const LEAF_POINTER_VALUE: u32 = 0xDEADBEEFu32;

pub const LEAF_ARRAY_HASH:   u64 = 0x797272416661654c_u64; // "LeafArry" in ASCII
pub const LEAF_RECORD_HASH:  u64 = 0x647263526661654c_u64; // "LeafRcrd" in ASCII
pub const NULL_POINTER_HASH: u64 = 0x726174536c6c754e_u64; // "NullStar" in ASCII
pub const LEAF_POINTER_HASH: u64 = 0x726174536661654c_u64; // "LeafStar" in ASCII
pub const VOID_POINTER_HASH: u64 = 0x7261745364696f56_u64; // "VoidStar" in ASCII
pub const FUNC_POINTER_HASH: u64 = 0x72617453636e7546_u64; // "FuncStar" in ASCII
pub const ANY_UNION_HASH:    u64 = 0x6e6f696e55796e41_u64; // "AnyUnion" in ASCII

// Hash implementation for slices
impl<'a, T: CrossCheckHash> CrossCheckHash for [T] {
    #[inline]
    fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
            where HA: CrossCheckHasher, HS: CrossCheckHasher {
        if depth == 0 {
            LEAF_ARRAY_HASH
        } else {
            let mut h = HA::default();
            for elem in self {
                let elem_hash = elem.cross_check_hash_depth::<HA, HS>(depth - 1);
                h.write_u64(elem_hash);
            }
            h.finish()
        }
    }
}

// Hash implementation for references
impl<'a, T: ?Sized + CrossCheckHash> CrossCheckHash for &'a T {
    #[inline]
    fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
            where HA: CrossCheckHasher, HS: CrossCheckHasher {
        if depth == 0 {
            CrossCheckHash::cross_check_hash_depth::<HA, HS>(&LEAF_REFERENCE_VALUE, 1)
        } else {
            // FIXME: don't decrease the depth when following references?
            (**self).cross_check_hash_depth::<HA, HS>(depth - 1)
        }
    }
}

impl<'a, T: ?Sized + CrossCheckHash> CrossCheckHash for &'a mut T {
    #[inline]
    fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
            where HA: CrossCheckHasher, HS: CrossCheckHasher {
        if depth == 0 {
            CrossCheckHash::cross_check_hash_depth::<HA, HS>(&LEAF_REFERENCE_VALUE, 1)
        } else {
            // FIXME: don't decrease the depth when following references?
            (**self).cross_check_hash_depth::<HA, HS>(depth - 1)
        }
    }
}

// Hash implementation for raw pointers
impl<T: ?Sized + CrossCheckHash> CrossCheckHash for *const T {
    #[inline]
    fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
            where HA: CrossCheckHasher, HS: CrossCheckHasher {
        let r = unsafe { self.as_ref() };
        match (r, depth) {
            (None, _) => NULL_POINTER_HASH,
            (_,    0) => LEAF_POINTER_HASH,
            // FIXME: even non-NULL pointers may be invalid
            (Some(r), _) => (*r).cross_check_hash_depth::<HA, HS>(depth - 1)
        }
    }
}

impl<T: ?Sized + CrossCheckHash> CrossCheckHash for *mut T {
    #[inline]
    fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
            where HA: CrossCheckHasher, HS: CrossCheckHasher {
        let r = unsafe { self.as_ref() };
        match (r, depth) {
            (None, _) => NULL_POINTER_HASH,
            (_,    0) => LEAF_POINTER_HASH,
            // FIXME: even non-NULL pointers may be invalid
            (Some(r), _) => (*r).cross_check_hash_depth::<HA, HS>(depth - 1)
        }
    }
}

macro_rules! impl_fnopt_hash {
    (<$($arg:ident),*> + $($pfx:tt)*) => {
        impl <Ret, $($arg),*> CrossCheckHash for $($pfx)* fn($($arg),*) -> Ret {
            #[inline]
            fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
                    where HA: CrossCheckHasher, HS: CrossCheckHasher {
                if depth == 0 {
                    LEAF_POINTER_HASH
                } else {
                    FUNC_POINTER_HASH
                }
            }
        }

        impl <Ret, $($arg),*> CrossCheckHash for Option<$($pfx)* fn($($arg),*) -> Ret> {
            #[inline]
            fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
                    where HA: CrossCheckHasher, HS: CrossCheckHasher {
                if let &Some(ref func) = self {
                    // Due to C's decay rules, we don't decrease the depth here,
                    // since function values can decay to function pointers,
                    // so they're basically equivalent
                    (*func).cross_check_hash_depth::<HA, HS>(depth)
                } else {
                    NULL_POINTER_HASH
                }
            }
        }
    };
    (<$($arg:ident),+, ...> + $($pfx:tt)*) => {
        impl <Ret, $($arg),+> CrossCheckHash for $($pfx)* fn($($arg),+, ...) -> Ret {
            #[inline]
            fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
                    where HA: CrossCheckHasher, HS: CrossCheckHasher {
                if depth == 0 {
                    LEAF_POINTER_HASH
                } else {
                    FUNC_POINTER_HASH
                }
            }
        }

        impl <Ret, $($arg),+> CrossCheckHash for Option<$($pfx)* fn($($arg),+, ...) -> Ret> {
            #[inline]
            fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
                    where HA: CrossCheckHasher, HS: CrossCheckHasher {
                if let &Some(ref func) = self {
                    // Due to C's decay rules, we don't decrease the depth here,
                    // since function values can decay to function pointers,
                    // so they're basically equivalent
                    (*func).cross_check_hash_depth::<HA, HS>(depth)
                } else {
                    NULL_POINTER_HASH
                }
            }
        }
    };
    () => {
        impl_fnopt_hash!(<> + unsafe extern "C");
    };
    ($($arg:ident),+) => {
        impl_fnopt_hash!(<$($arg),+> + unsafe extern "C");
        impl_fnopt_hash!(<$($arg),+, ...> + unsafe extern "C");
    }
}

// Default CrossCheckHash implementation for function pointers with up to 12 arguments
// FIXME: this is incredibly ugly, but there is no cleaner way
// until Rust gets variadic generics
impl_fnopt_hash!();
impl_fnopt_hash!(A);
impl_fnopt_hash!(A, B);
impl_fnopt_hash!(A, B, C);
impl_fnopt_hash!(A, B, C, D);
impl_fnopt_hash!(A, B, C, D, E);
impl_fnopt_hash!(A, B, C, D, E, F);
impl_fnopt_hash!(A, B, C, D, E, F, G);
impl_fnopt_hash!(A, B, C, D, E, F, G, H);
impl_fnopt_hash!(A, B, C, D, E, F, G, H, I);
impl_fnopt_hash!(A, B, C, D, E, F, G, H, I, J);
impl_fnopt_hash!(A, B, C, D, E, F, G, H, I, J, K);
impl_fnopt_hash!(A, B, C, D, E, F, G, H, I, J, K, L);

macro_rules! impl_array_hash {
    ($($N:expr)+) => { $(
        impl<T: CrossCheckHash> CrossCheckHash for [T; $N] {
            #[inline]
            fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
                    where HA: CrossCheckHasher, HS: CrossCheckHasher {
                self[..].cross_check_hash_depth::<HA, HS>(depth)
            }
        }
    )+ }
}

#[cfg(feature="fixed-length-array-hash")]
impl_array_hash!{
    // Values from 0 to 32
     0  1  2  3  4  5  6  7  8  9 10 11 12 13 14
    15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
    // Powers of two
      31   32   33
      63   64   65
     127  128  129
     255  256  257
     511  512  513
    1023 1024 1025
}

// Helper macro that implements CrossCheckHash for a fixed-size array with
// a given type and a list of sizes, i.e., `[T; N]` for several values of N
#[macro_export]
macro_rules! cross_check_hash_array {
    [$ET:ty; [$($N:expr),+]] => { $(
        impl $crate::hash::CrossCheckHash for [$ET; $N] {
            #[inline]
            fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
                    where HA: $crate::hash::CrossCheckHasher,
                          HS: $crate::hash::CrossCheckHasher {
                self[..].cross_check_hash_depth::<HA, HS>(depth)
            }
        }
    )+ }
}

#[cfg(feature="libc-hash")]
impl CrossCheckHash for libc::c_void {
    #[inline]
    fn cross_check_hash<HA, HS>(&self) -> Option<u64>
            where HA: CrossCheckHasher, HS: CrossCheckHasher {
        None
    }

    #[inline]
    fn cross_check_hash_depth<HA, HS>(&self, _depth: usize) -> u64
            where HA: CrossCheckHasher, HS: CrossCheckHasher {
        // FIXME: this returns the correct value for a void*
        // parent pointer, but is wrong for a void-typed
        // structure field or array element
        VOID_POINTER_HASH
    }
}
