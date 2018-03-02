extern crate libc;

use bool::sieve_of_eratosthenes as rust_sieve_of_eratosthenes;
use self::libc::c_int;

extern "C" {
    #[no_mangle]
    fn sieve_of_eratosthenes(_: *mut c_int);
}

pub fn test_buffer() {
    panic!("TESTME");
}
