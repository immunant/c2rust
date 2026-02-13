#![allow(unused)]

extern "C" {
    #[no_mangle]
    fn abort() -> !;
    #[no_mangle]
    fn exit(status: i32) -> !;
}

fn test_abort() {
    unsafe {
        std::process::abort();
    }
}

fn test_exit() {
    unsafe {
        std::process::exit(0);
    }
}

fn test_exit_error() {
    unsafe {
        std::process::exit(1);
    }
}

fn test_exit_variable() {
    let code = 42;
    unsafe {
        std::process::exit(code);
    }
}

fn main() {
    println!("This won't actually run");
}
