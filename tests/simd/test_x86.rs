extern crate libc;

use x86::{rust_unpack_128_2x128, rust_zero_init_all};

#[cfg(target_arch = "x86")]
use std::arch::x86::{__m128i, _mm_setzero_si128, _mm_set_epi32};
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::{__m128i, _mm_setzero_si128, _mm_set_epi32};
use std::mem::transmute;

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn unpack_128_2x128(data: __m128i, data_lo: *mut __m128i, data_hi: *mut __m128i);
}

pub fn test_unpack_128_2x128() {
    // Apparently these ops are very unsafe to call when
    // you don't actually have support for them
    assert!(is_x86_feature_detected!("sse2"));

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
    unsafe {
        rust_zero_init_all();
    }
}
