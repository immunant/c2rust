
// `const`-ness of variable bindings (both in function arguments and locals) in C corresponds
// directly to `mut`-ness of pattern bindings in Rust.
int constant_arguments(int const x /* should be 'x: libc::c_int' */) {
    int const y = x + 2;           // should be 'y: libc::c_int'
    return y;
}

// Still just a `const` argument - nothing special here
void constant_pointer(int *const x /* should be 'x: *mut libc::c_int' */) {
    *x += 1;
}

// `const`-ness of pointee type in C corresponds directly to `const`/`mut` pointers in Rust
int pointer_to_constant(int const *x /* should be 'mut x: *const libc::c_int' */) {
    return x[1] + 1;
}

void entry(unsigned buffer_size, int buffer[])
{
    if (buffer_size >= 5) {
        buffer[0] = constant_arguments(1);
        constant_pointer(&buffer[1]);
        buffer[1] = pointer_to_constant(buffer);
    }
}

