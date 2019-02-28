static __attribute__((always_inline)) void always_inline_static(void) {}
// static __attribute__((gnu_inline)) void inline gnu_inline_foo(void) {}
static __attribute__((noinline)) void never_inline_static(void) {}
// static void inline inline_foo(void) {}
__attribute__((always_inline)) void always_inline_nonstatic(void) {}
// __attribute__((gnu_inline)) void inline gnu_inline_pub(void) {}
__attribute__((noinline)) void never_inline_nonstatic(void) {}
// void inline inline_pub(void) {}

void ensure_use(void) {
    always_inline_static();
    always_inline_nonstatic();
    // gnu_inline_foo();
    never_inline_static();
    // inline_foo();
}
