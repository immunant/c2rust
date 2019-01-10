// JodyHash: fast hash function for 64-bit blocks
// from https://github.com/jbruchon/jodyhash

use super::CrossCheckHasher;
use std::hash::Hasher;
use std::ops::BitXor;

#[derive(Debug, Default)]
pub struct JodyHasher(u64);

const JODY_HASH_CONSTANT: u64 = 0x1f3d_5b79u64;
const JODY_HASH_SHIFT: u32 = 14;

impl Hasher for JodyHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, _bytes: &[u8]) {
        // FIXME: we only support 64-bit inputs for now
        unimplemented!()
    }

    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.0 = self
            .0
            .wrapping_add(i)
            .wrapping_add(JODY_HASH_CONSTANT)
            .rotate_left(JODY_HASH_SHIFT)
            .bitxor(i)
            .rotate_left(JODY_HASH_SHIFT)
            .bitxor(JODY_HASH_CONSTANT)
            .wrapping_add(i);
    }
}

impl CrossCheckHasher for JodyHasher {}

#[cfg(test)]
mod tests {
    use super::{Hasher, JodyHasher};

    fn jodyhash_string(s: &str) -> u64 {
        assert_eq!(s.len() % 8, 0);
        let mut h = JodyHasher::default();
        let sptr = s.as_ptr() as *const u64;
        for i in 0..s.len() / 8 {
            h.write_u64(unsafe { *sptr.offset(i as isize) })
        }
        h.finish()
    }

    #[test]
    fn test_jodyhash() {
        // Single word strings
        assert_eq!(jodyhash_string("aaaaaaaa"), 0xa1b314f742d47698u64);
        assert_eq!(jodyhash_string("abcdefgh"), 0xea0ab92ac586d967u64);
        assert_eq!(jodyhash_string("hgfedcba"), 0x31f588dbf657cc7fu64);
        assert_eq!(jodyhash_string("jodyhash"), 0xba43645fabc566ddu64);

        // 2-word strings
        assert_eq!(jodyhash_string("aaaaaaaaaaaaaaaa"), 0x35d0ccd0c8b5a5a5u64);
        assert_eq!(jodyhash_string("abcdefghijklmnop"), 0xcef9115bbc6c0f84u64);
        assert_eq!(jodyhash_string("zyxwvutsrqponmlk"), 0xe408a517f5c12d07u64);
    }

    // Test that checks the correctness of struct7.c in clang-plugin/test
    #[test]
    fn test_clang_plugin_struct7() {
        let mut h = JodyHasher::default();
        h.write_u64(06632337_u64);
        h.write_u64(00001337_u64 ^ 0x7878787878787876_u64);
        assert_eq!(h.finish(), 0xaf19d4dba0422cc0_u64);
    }

    // A more complex test for JodyHash
    // Test case taken from a cross-check failure in libxml2:
    // hash of xmlError structure differs between C and Rust
    #[test]
    fn test_jodyhash_libxml2_xmlerror() {
        let mut h = JodyHasher::default();
        h.write_u64(005_u64 ^ 0x7878787878787876_u64); //   domain = 5
        h.write_u64(068_u64 ^ 0x7878787878787876_u64); //     code = 68
        h.write_u64(104_u64 ^ 0xc3c3c3c3c3c3c3c2_u64); // *message = 104
        h.write_u64(002_u64 ^ 0xb4b4b4b4b4b4b4b4_u64); //    level = 2
        h.write_u64(116_u64 ^ 0xc3c3c3c3c3c3c3c2_u64); //    *file = 116
        h.write_u64(975_u64 ^ 0x7878787878787876_u64); //     line = 975
        h.write_u64(0x726174536c6c754e_u64); //     str1 = NULL
        h.write_u64(0x726174536c6c754e_u64); //     str2 = NULL
        h.write_u64(0x726174536c6c754e_u64); //     str3 = NULL
        h.write_u64(000_u64 ^ 0x7878787878787876_u64); //     int1 = 0
        h.write_u64(002_u64 ^ 0x7878787878787876_u64); //     int2 = 2
        h.write_u64(0x7261745364696f56_u64); //     ctxt = void*
        h.write_u64(0x726174536c6c754e_u64); //     node = NULL
        assert_eq!(h.finish(), 0x816be1ca90dc62b0_u64);
    }
}
