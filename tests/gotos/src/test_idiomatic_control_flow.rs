extern crate libc;

use idiomatic_switch::rust_idiomatic_switch;
use idiomatic_nested_loops::rust_break_multiple;

pub fn test_idiomatic_switch() {
    unsafe {
        assert_eq!(rust_idiomatic_switch(-1), 1);
        assert_eq!(rust_idiomatic_switch(0), 1);
        assert_eq!(rust_idiomatic_switch(1), 3);
        assert_eq!(rust_idiomatic_switch(2), 5);
    }
}

pub fn test_break_multiple_loops() {
    unsafe {
        assert_eq!(rust_break_multiple(0), 4);
        assert_eq!(rust_break_multiple(1), 5);
        assert_eq!(rust_break_multiple(3), 9);
        assert_eq!(rust_break_multiple(4), 9);
        assert_eq!(rust_break_multiple(6), 10);
    }
}
