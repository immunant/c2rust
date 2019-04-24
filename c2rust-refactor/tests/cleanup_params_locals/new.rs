fn with_params(_unused: i32, mut used: i32, _unused2: i32, used_immut: i32) {
    used += used_immut;
}
