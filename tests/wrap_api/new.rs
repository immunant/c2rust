pub unsafe fn api() {}
#[export_name = "api"]
pub unsafe extern "C" fn api_wrapper() {
    api()
}

pub unsafe fn api_two(x: i32, y: i32) {}
#[export_name = "api2"]
pub unsafe extern "C" fn api2_wrapper(x: i32, y: i32) {
    api_two(x, y)
}

pub unsafe fn api3(_: i32, y: i32) {}
#[export_name = "api3"]
pub unsafe extern "C" fn api3_wrapper(arg0: i32, y: i32) {
    api3(arg0, y)
}

pub unsafe fn api4(x: i32, _: i32) {}
#[export_name = "api4"]
pub unsafe extern "C" fn api4_wrapper(x: i32, arg1: i32) {
    api4(x, arg1)
}

pub unsafe fn api5(_: i32, _: i32) {}
#[export_name = "api5"]
pub unsafe extern "C" fn api5_wrapper(arg0: i32, arg1: i32) {
    api5(arg0, arg1)
}

pub unsafe fn api6(_: i32, arg0: i32) {}
#[export_name = "api6"]
pub unsafe extern "C" fn api6_wrapper(arg0: i32, arg0_0: i32) {
    api6(arg0, arg0_0)
}


fn main() {
    unsafe {
        api();
        api_two(2, 2);
    }
}
