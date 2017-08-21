unsafe fn not_unsafe() -> i32 {
    0
}

unsafe fn really_unsafe() -> i32 {
    *(0_usize as *const i32)
}

fn very_safe() -> i32 {
    {
        0
    }
}

fn not_unsafe_caller() -> i32 {
    unsafe { not_unsafe() }
}

fn really_unsafe_caller() -> i32 {
    unsafe { really_unsafe() }
}

fn main() {
    let sum = very_safe() + not_unsafe_caller() + really_unsafe_caller();
    println!("sum = {}", sum);
}
