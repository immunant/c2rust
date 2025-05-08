
// Typedefs should "forget" about their qualifiers for the type synonym
typedef int my_int;                          // 'type my_int = std::ffi::c_int'
typedef my_int * int_ptr;                    // 'type int_ptr = *mut my_int'
typedef int_ptr const const_int_ptr;         // 'type const_int_ptr = int_ptr'
typedef const_int_ptr indirectly_const_ptr;  // 'type indirectly_const_ptr = const_int_ptr'
typedef int (my_fn)(int i);                  // 'type my_fn = fn(std::ffi::c_int) -> std::ffi::c_int'

int identity(int x) { return x; }

// The qualifiers should still be extracted from typedefs when needed (for example at binding
// sites)
int entry(void)
{
    my_int x = 1;                            // 'mut x: my_int'
    my_int const y = 1;                      // 'y: my_int'
    int_ptr z = &x;                          // 'mut z: int_ptr'
    const_int_ptr w = &x;                    // 'w: const_int_ptr'
    const const_int_ptr v = &x;              // 'v: const_int_ptr'
    indirectly_const_ptr u = &x;             // 'u: indirectly_const_ptr'
    my_fn *t = &identity;                    // 't: my_fn: fn(std::ffi::c_int) -> std::ffi::c_int' 

    typedef int shadowed;
    shadowed n = 1;
    {
        typedef long shadowed;
        shadowed n = 2;
    }
    return 0;
}

