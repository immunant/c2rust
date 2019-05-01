fn with_params(_unused: i32, mut used: i32, _unused2: i32, used_immut: i32, _unused: i32) {
    let _unused3 = 1;
    let used2 = 2;
    used += used_immut + used2;
}

fn used_in_local(p1: i32, mut p2: i32) {
    let _unused = p1;
    p2 = p1;
}
