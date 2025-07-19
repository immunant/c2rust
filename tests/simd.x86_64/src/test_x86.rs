use crate::x86::{
    rust_call_all, rust_call_all_used, rust_simd_fn_codegen, rust_static_m128, rust_static_m128d,
    rust_static_m128i, rust_static_m256, rust_static_m256d, rust_static_m256i,
    rust_static_uninit_m128, rust_static_uninit_m128d, rust_static_uninit_m128i,
    rust_static_uninit_m256, rust_static_uninit_m256d, rust_static_uninit_m256i,
    rust_unpack_128_2x128, rust_vector_init_lists, rust_vector_init_lists_used, rust_zero_init_all,
    ShuffleVectors, VectorInitLists,
};

#[cfg(target_arch = "x86")]
use std::arch::x86::{
    __m128, __m128d, __m128i, __m256, __m256d, __m256i, _mm_set_epi32, _mm_setzero_si128,
};
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::{
    __m128, __m128d, __m128i, __m256, __m256d, __m256i, _mm_set_epi32, _mm_setzero_si128,
};
use std::fmt::{Debug, Error, Formatter};
use std::mem::transmute;

// Our travis-ci machines don't support AVX2 so we conditionally compile those bits out

#[link(name = "test")]
extern "C" {
    fn unpack_128_2x128(data: __m128i, data_lo: *mut __m128i, data_hi: *mut __m128i);

    fn call_all() -> ShuffleVectors;

    fn call_all_used() -> ShuffleVectors;

    fn vector_init_lists() -> VectorInitLists;

    fn vector_init_lists_used() -> VectorInitLists;

    static static_m128: __m128;

    static static_m256: __m256;

    static static_m128d: __m128d;

    static static_m256d: __m256d;

    static static_m128i: __m128i;

    static static_m256i: __m256i;
}

static UNSAFETY_ERROR: &str =
    "Prevented unsafe calling of SIMD functions when architecture support doesn't exist";

macro_rules! cmp_vector_fields {
    ($this: ident, $other: ident: [
        $($(#[$attrs:meta])* $field: ident : $typ: ty),+,
    ]) => {
        $(
            $(#[$attrs])*
            {
                let self_vec: $typ = unsafe { transmute($this.$field) };
                let other_vec: $typ = unsafe { transmute($other.$field) };

                if self_vec != other_vec {
                    eprintln!("fields {}: ({:?}) != ({:?})", stringify!($field), self_vec, other_vec);
                    return false
                }
            }
        )+
    };
}

impl PartialEq for ShuffleVectors {
    fn eq(&self, other: &ShuffleVectors) -> bool {
        cmp_vector_fields!(self, other: [
            #[cfg(target_feature = "mmx")]
            a: u64,
            b: u128,
            c: u128,
            d: (u128, u128),
            ep: (u128, u128),
            e: (u128, u128),
            f: u128,
            g: u128,
            h: u128,
            #[cfg(target_feature = "avx2")]
            i: (u128, u128),
            #[cfg(target_feature = "avx2")]
            j: (u128, u128),
            #[cfg(target_feature = "avx2")]
            k: (u128, u128),
            #[cfg(target_feature = "mmx")]
            l: u64,
            m: u128,
            #[cfg(target_feature = "avx2")]
            n: (u128, u128),
            o: u128,
            #[cfg(target_feature = "avx2")]
            p: (u128, u128),
            #[cfg(target_feature = "avx2")]
            q: (u128, u128),
            r: u128,
        ]);

        return true;
    }
}

impl Debug for ShuffleVectors {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "ShuffleVector {{ .. }}")
    }
}

#[test]
pub fn test_unpack_128_2x128() {
    assert!(is_x86_feature_detected!("sse2"), "{}", UNSAFETY_ERROR);

    unsafe {
        let data = _mm_setzero_si128();
        let mut data2 = _mm_set_epi32(1, 2, 3, 4);
        let mut data3 = _mm_set_epi32(5, 6, 7, 8);
        let mut data4 = _mm_set_epi32(1, 2, 3, 4);
        let mut data5 = _mm_set_epi32(5, 6, 7, 8);

        rust_unpack_128_2x128(data, &mut data2, &mut data3);
        unpack_128_2x128(data, &mut data4, &mut data5);

        // A __m128i is a set of four 32bit data
        let (r1, r2, r3, r4): (u32, u32, u32, u32) = transmute(data2);
        let (r5, r6, r7, r8): (u32, u32, u32, u32) = transmute(data3);
        let (c1, c2, c3, c4): (u32, u32, u32, u32) = transmute(data4);
        let (c5, c6, c7, c8): (u32, u32, u32, u32) = transmute(data5);

        assert_eq!(r1, c1);
        assert_eq!(r2, c2);
        assert_eq!(r3, c3);
        assert_eq!(r4, c4);
        assert_eq!(r5, c5);
        assert_eq!(r6, c6);
        assert_eq!(r7, c7);
        assert_eq!(r8, c8);
    }
}

#[test]
pub fn test_zero_initializers() {
    assert!(is_x86_feature_detected!("sse"), "{}", UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("sse2"), "{}", UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("avx"), "{}", UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("mmx"), "{}", UNSAFETY_ERROR);

    unsafe {
        rust_zero_init_all();
    }
}

#[test]
pub fn test_shuffle_vectors() {
    assert!(is_x86_feature_detected!("sse4.2"), "{}", UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("ssse3"), "{}", UNSAFETY_ERROR);

    let c1 = unsafe { call_all() };
    let c2 = unsafe { call_all_used() };
    let r1 = unsafe { rust_call_all() };
    let r2 = unsafe { rust_call_all_used() };

    assert_eq!(c1, r1);
    assert_eq!(c2, r2);
}

impl PartialEq for VectorInitLists {
    fn eq(&self, other: &VectorInitLists) -> bool {
        cmp_vector_fields!(self, other: [
            a: (f32, f32, f32, f32),
            b: (f32, f32, f32, f32, f32, f32, f32, f32),
            c: (f64, f64),
            d: (f64, f64, f64, f64),
            e: (i64, i64),
            f: (i64, i64, i64, i64),
        ]);

        return true;
    }
}

impl Debug for VectorInitLists {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "VectorInitLists {{ .. }}")
    }
}

#[test]
pub fn test_vector_init_lists() {
    assert!(is_x86_feature_detected!("sse"), "{}", UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("sse2"), "{}", UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("avx"), "{}", UNSAFETY_ERROR);

    let c1 = unsafe { vector_init_lists() };
    let c2 = unsafe { vector_init_lists_used() };
    let r1 = unsafe { rust_vector_init_lists() };
    let r2 = unsafe { rust_vector_init_lists_used() };

    assert_eq!(c1, r1);
    assert_eq!(c2, r2);
}

#[test]
pub fn test_static_init_lists() {
    assert!(is_x86_feature_detected!("sse"), "{}", UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("sse2"), "{}", UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("avx"), "{}", UNSAFETY_ERROR);

    // These should have been transmuted due to being in statics
    let rust_static_init_lists = unsafe {
        VectorInitLists {
            a: rust_static_m128,
            b: rust_static_m256,
            c: rust_static_m128d,
            d: rust_static_m256d,
            e: rust_static_m128i,
            f: rust_static_m256i,
        }
    };

    let static_init_lists = unsafe {
        VectorInitLists {
            a: static_m128,
            b: static_m256,
            c: static_m128d,
            d: static_m256d,
            e: static_m128i,
            f: static_m256i,
        }
    };

    assert_eq!(static_init_lists, rust_static_init_lists);
}
