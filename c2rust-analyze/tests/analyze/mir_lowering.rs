#![deny(unsafe_op_in_unsafe_fn)]

// These expressions exercise MIR whose spans are shared by different HIR
// nodes: an `as` coercion, a conditional call, and a desugared while loop.
unsafe fn read_if_present(p: *const i32) -> i32 {
    if !p.is_null() {
        unsafe { *p }
    } else {
        0
    }
}

unsafe fn read_from_mut(p: *mut i32) -> i32 {
    unsafe { read_if_present(p as *const i32) }
}

struct Array {
    values: [i32; 4],
}

fn array_sum(array: &Array) -> i32 {
    let mut total = 0;
    let mut i = 0;
    while i < 4 {
        let element: *const i32 = &(*array).values[i];
        total += unsafe { *element };
        i += 1;
    }
    total
}

fn slice_sum(values: &mut [i32]) -> i32 {
    macro_rules! element {
        ($values:expr, $index:expr) => {
            $values[$index]
        };
    }
    let mut total = 0;
    let mut i = 0;
    while i < values.len() {
        values[i] += 1;
        total += element!(values, i);
        i += 1;
    }
    total
}

fn invoke(f: for<'a> fn(&'a i32) -> i32, value: &i32) -> i32 {
    f(value)
}

fn copy(value: &i32) -> i32 {
    *value
}

fn verify(condition: bool) {
    if !condition {
        std::process::exit(1);
    }
}

fn main() {
    let mut value = 17;
    verify(unsafe { read_from_mut(&raw mut value) } == 17);
    verify(unsafe { read_if_present(std::ptr::null()) } == 0);
    let mut values = [1, 2, 3, 4];
    verify(array_sum(&Array { values }) == 10);
    verify(slice_sum(&mut values) == 14);
    verify(values == [2, 3, 4, 5]);
    verify(invoke(copy, &value) == 17);
}
