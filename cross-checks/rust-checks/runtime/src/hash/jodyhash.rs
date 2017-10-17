// JodyHash: fast hash function for 64-bit blocks
// from https://github.com/jbruchon/jodyhash

use std::hash::Hasher;
use std::ops::BitXor;
use super::XCheckHasher;

#[derive(Debug, Default)]
pub struct JodyHasher(u64);

impl JodyHasher {
    #[inline]
    pub fn new() -> JodyHasher {
        Default::default()
    }
}

const JODY_HASH_CONSTANT: u64 = 0x1f3d5b79u64;
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
        self.0 = self.0
            .wrapping_add(i)
            .wrapping_add(JODY_HASH_CONSTANT)
            .rotate_left(JODY_HASH_SHIFT)
            .bitxor(i)
            .rotate_left(JODY_HASH_SHIFT)
            .bitxor(JODY_HASH_CONSTANT)
            .wrapping_add(i);
    }
}

impl XCheckHasher for JodyHasher {}

#[cfg(test)]
mod tests {
    use super::{Hasher, JodyHasher};

    #[test]
    fn test_jodyhash() {
        // TODO
    }
}
