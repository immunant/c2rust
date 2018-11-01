extern crate libc;

use x86::{ShuffleVectors, VectorInitLists, rust_unpack_128_2x128, rust_zero_init_all, rust_call_all, rust_call_all_used, rust_vector_init_lists, rust_vector_init_lists_used};

#[cfg(target_arch = "x86")]
use std::arch::x86::{__m128i, _mm_setzero_si128, _mm_set_epi32};
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::{__m128i, _mm_setzero_si128, _mm_set_epi32};
use std::mem::transmute;
use std::fmt::{Debug, Formatter, Error};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn unpack_128_2x128(data: __m128i, data_lo: *mut __m128i, data_hi: *mut __m128i);

    #[no_mangle]
    fn call_all() -> ShuffleVectors;

    #[no_mangle]
    fn call_all_used() -> ShuffleVectors;

    #[no_mangle]
    fn vector_init_lists() -> VectorInitLists;

    #[no_mangle]
    fn vector_init_lists_used() -> VectorInitLists;
}

static UNSAFETY_ERROR: &str = "Prevented unsafe calling of SIMD functions when architecture support doesn't exist";

macro_rules! cmp_vector_fields {
    ($this: ident, $other: ident: [$($field: ident : $typ: ty),+,]) => {
        $(
            let self_vec: $typ = unsafe { transmute($this.$field) };
            let other_vec: $typ = unsafe { transmute($other.$field) };

            if self_vec != other_vec {
                eprintln!("({:?}) != ({:?})", self_vec, other_vec);
                return false
            }
        )+
    };
}

impl PartialEq for ShuffleVectors {
    fn eq(&self, other: &ShuffleVectors) -> bool {
        cmp_vector_fields!(self, other: [
            a: u64,
            b: u128,
            c: u128,
            d: (u128, u128),
            e: (u128, u128),
            f: u128,
            g: u128,
            h: u128,
            i: (u128, u128),
            j: (u128, u128),
            k: (u128, u128),
            l: u64,
            m: u128,
            n: (u128, u128),
        ]);

        return true;
    }
}

impl Debug for ShuffleVectors {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "ShuffleVector {{ .. }}")
    }
}

pub fn test_unpack_128_2x128() {
    assert!(is_x86_feature_detected!("sse2"), UNSAFETY_ERROR);

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

pub fn test_zero_initializers() {
    assert!(is_x86_feature_detected!("sse"), UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("sse2"), UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("avx"), UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("mmx"), UNSAFETY_ERROR);

    unsafe {
        rust_zero_init_all();
    }
}

pub fn test_shuffle_vectors() {
    assert!(is_x86_feature_detected!("sse4.2"), UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("ssse3"), UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("avx2"), UNSAFETY_ERROR);

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

pub fn test_vector_init_lists() {
    assert!(is_x86_feature_detected!("sse"), UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("sse2"), UNSAFETY_ERROR);
    assert!(is_x86_feature_detected!("avx"), UNSAFETY_ERROR);

    let c1 = unsafe { vector_init_lists() };
    let c2 = unsafe { vector_init_lists_used() };
    let r1 = unsafe { rust_vector_init_lists() };
    let r2 = unsafe { rust_vector_init_lists_used() };

    assert_eq!(c1, r1);
    assert_eq!(c2, r2);
}
