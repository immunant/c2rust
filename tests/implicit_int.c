// Should fail
// Due to a quirk in the C language (at least as handled by clang), 
// we need special case handling for functions that are declared without
// a prototype and use implicit typing of their parameters. See
// https://github.com/GaloisInc/C2Rust/issues/32
typedef int (my_fn)(int i);        

int identity(x)
{
    return x;
}

void entry(unsigned buffer_size, int buffer[])
{
    // assign to t using the address-of operator
    my_fn *t = &identity;                    // 't: my_fn: fn(libc::c_int) -> libc::c_int'
    // assign to u using fuction to pointer decay
    my_fn *u = identity;                    // 't: my_fn: fn(libc::c_int) -> libc::c_int'
}

