
// forward declaration of structs should not cause problems.
struct s;
struct forward;
struct s { int x; };

void forward(const unsigned int buffer_size, int buffer[const]){
    if (buffer_size < 1) return;

    struct s foo = { 1 };
    buffer[0] = foo.x;

    // GH #84: Previously could not initialize null ptrs
    // for forward declared structs which do not have a definition
    struct forward *initialized_null_ptr;
}

