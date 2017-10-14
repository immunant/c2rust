
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

#[cfg(test)]
mod tests {
    use super::Hasher;
    use super::Djb2Hasher;

    fn djb2_string(s: &str) -> u32 {
        let mut h = Djb2Hasher::new();
        h.write(s.as_bytes());
        h.get_hash()
    }

    #[test]
    fn test_djb2() {
        assert_eq!(djb2_string("a"),        0x0002b606u32);
        assert_eq!(djb2_string("ab"),       0x00597728u32);
        assert_eq!(djb2_string("abc"),      0x0b885c8bu32);
        assert_eq!(djb2_string("abcd"),     0x7c93ee4fu32);
        assert_eq!(djb2_string("abcde"),    0x0f11b894u32);
        assert_eq!(djb2_string("abcdef"),   0xf148cb7au32);
        assert_eq!(djb2_string("abcdefg"),  0x1a623b21u32);
        assert_eq!(djb2_string("abcdefgh"), 0x66a99fa9u32);
        assert_eq!(djb2_string("djb2"),     0x7c95b527u32);
    }
}
