use casts::cast_stuff as rust_cast_stuff;

extern "C" {
    #[no_mangle]
    fn cast_stuff();
}

pub fn test_compiles() {
    unsafe {
        cast_stuff();
        rust_cast_stuff();
    }
}
