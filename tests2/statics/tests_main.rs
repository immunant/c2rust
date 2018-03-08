#![feature(libc)]
#![feature(i128_type)]

pub mod test_storage;
pub mod storage;


pub fn main() {
    match std::env::args().nth(1).as_ref().map(String::as_ref) {
        Some("test_storage::test_buffer") => test_storage::test_buffer(),
        e => panic!("Tried to run unknown test: {:?}", e),
    }
}

