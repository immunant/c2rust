
// `const`-ness of variable bindings (both in function arguments and locals) in C corresponds
// directly to `mut`-ness of pattern bindings in Rust.
int constant_arguments(int const x /* should be 'x: std::ffi::c_int' */) {
    int const y = x + 2;           // should be 'y: std::ffi::c_int'
    return y;
}

// Still just a `const` argument - nothing special here
void constant_pointer(int *const x /* should be 'x: *mut std::ffi::c_int' */) {
    *x += 1;
}

// `const`-ness of pointee type in C corresponds directly to `const`/`mut` pointers in Rust
int pointer_to_constant(int const *x /* should be 'mut x: *const std::ffi::c_int' */) {
    return x[1] + 1;
}

void entry4(const unsigned int buffer_size, int buffer[])
{
    buffer[0] = constant_arguments(1);
    constant_pointer(&buffer[1]);
    buffer[1] = pointer_to_constant(buffer);
}

// Check that taking the address of a pointer works correctly with
// constant and mutable variables. This is a compilation check to
// ensure that the correct & and &mut variants are used.
void addr_of_const(void) {
        const int ci = 0;
        int i = 0;

        const int *p1 = &ci;
        const int *p2 = &i;
        int *p3 = &i;
}
