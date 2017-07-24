#[no_mangle]
pub unsafe extern "C" fn api() {
}

#[export_name="api2"]
pub unsafe extern "C" fn api_two(x: i32, y: i32) {
}

#[no_mangle]
pub unsafe extern "C" fn api3(_: i32, y: i32) {
}

#[no_mangle]
pub unsafe extern "C" fn api4(x: i32, _: i32) {
}

#[no_mangle]
pub unsafe extern "C" fn api5(_: i32, _: i32) {
}

#[no_mangle]
pub unsafe extern "C" fn api6(_: i32, arg0: i32) {
}


fn main() {
    unsafe {
        api();
        api_two(2, 2);
    }
}
