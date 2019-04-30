fn with_params(mut unused: i32, mut used: i32, mut unused2: i32, mut used_immut: i32, mut _unused: i32) {
    used += used_immut;
}
