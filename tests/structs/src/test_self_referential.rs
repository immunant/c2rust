extern crate libc;

use crate::self_referential::Node;

#[link(name = "test")]
extern "C" {
    fn whatever(np: *mut Node);
}

#[cfg_attr(test, test)]
pub fn test_buffer2() {
    
}
