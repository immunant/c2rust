extern crate libc;

use qsort::{rust_partition, rust_quickSort, rust_swap};
use self::libc::c_int;

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn swap(_: *mut c_int, _: *mut c_int);

    #[no_mangle]
    fn partition(_: *mut c_int, _: c_int, _: c_int);

    #[no_mangle]
    fn quickSort(_: *mut c_int, _: c_int, _: c_int);
}

pub fn test_swap() {
    let (mut a, mut b) = (1, 2);

    unsafe {
        swap(&mut a, &mut b);
    }

    assert_eq!(a, 2);
    assert_eq!(b, 1);

    unsafe {
        rust_swap(&mut a, &mut b)
    }

    assert_eq!(a, 1);
    assert_eq!(b, 2);
}

pub fn test_partition() {
    let mut buffer = [6, 1, 5, 6, 2, 0, 9, 2, 0, 5];
    let mut rust_buffer = buffer.clone();
    let expected_buffer = [1, 5, 2, 0, 2, 0, 5, 6, 6, 9];

    unsafe {
        partition(buffer.as_mut_ptr(), 0, 9);
        rust_partition(rust_buffer.as_mut_ptr(), 0, 9);
    }

    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}

pub fn test_quicksort() {
    let mut buffer = [6, 1, 5, 6, 2, 0, 9, 2, 0, 5];
    let mut rust_buffer = buffer.clone();
    let expected_buffer = [0, 0, 1, 2, 2, 5, 5, 6, 6, 9];

    let (i1, i2) = unsafe {
        let i1 = quickSort(buffer.as_mut_ptr(), 0, 9);
        let i2 = rust_quickSort(rust_buffer.as_mut_ptr(), 0, 9);

        (i1, i2)
    };

    assert_eq!(i1, i2);
    assert_eq!(buffer, rust_buffer);
    assert_eq!(buffer, expected_buffer);
}
