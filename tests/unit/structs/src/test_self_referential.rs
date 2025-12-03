use crate::self_referential::Node;

#[link(name = "test")]
extern "C" {
    fn whatever(np: *mut Node);
}

#[test]
pub fn test_buffer2() {}
