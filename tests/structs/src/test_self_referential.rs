use crate::self_referential::Node;

#[link(name = "test")]
extern "C" {
    fn whatever(np: *mut Node);
}

pub fn test_buffer2() {}
