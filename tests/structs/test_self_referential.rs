extern crate libc;

use self_referential::Node;

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn whatever(np: *mut Node);
}

pub fn test_buffer2() {
    
}
