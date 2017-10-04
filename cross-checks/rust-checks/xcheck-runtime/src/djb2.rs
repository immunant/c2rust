
use std::hash::Hasher;

#[derive(Debug)]
pub struct Djb2Hasher(u32);

impl Djb2Hasher {
    #[inline]
    pub fn new() -> Djb2Hasher {
        Djb2Hasher(5381u32)
    }

    #[inline]
    pub fn get_hash(&self) -> u32 {
        self.0
    }
}

impl Default for Djb2Hasher {
    #[inline]
    fn default() -> Djb2Hasher {
        Djb2Hasher::new()
    }
}

impl Hasher for Djb2Hasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0 as u64
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        self.0 = bytes.iter().fold(self.0,
            |h, c| h.wrapping_mul(33).wrapping_add(*c as u32));
    }
}
