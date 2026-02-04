#![allow(unused)]

extern "C" {
    #[no_mangle]
    fn abort() -> !;
    #[no_mangle]
    fn exit(status: i32) -> !;
}

// Custom abort function that should NOT be transformed
mod custom {
    #[no_mangle]
    pub extern "C" fn abort() -> ! {
        loop {}
    }
}

fn test_custom_abort() {
    unsafe {
        custom::abort();
    }
}

fn main() {
    println!("This won't actually run");
}
