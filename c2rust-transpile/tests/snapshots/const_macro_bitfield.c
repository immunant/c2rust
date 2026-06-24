// A const-like macro initializing a bitfield struct must be inlined at each
// use site, not emitted as a `const` (bitfield setters aren't `const`).

struct indexwriter {
    unsigned int should_write : 1;
    int fd;
};

#define INDEXWRITER_INIT { 0, 0 }

struct indexwriter make_writer(void) {
    struct indexwriter writer = INDEXWRITER_INIT;
    return writer;
}
