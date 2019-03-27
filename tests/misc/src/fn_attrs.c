static __attribute__((always_inline)) void always_inline_static(void) {}
static __attribute__((__noinline__)) void noinline_static(void) {}
static void inline inline_static(void) {}
static void inline __attribute__((__gnu_inline__)) gnu_inline_static(void) {}
void __attribute__((__always_inline__)) always_inline_nonstatic(void) {}
void inline __attribute__((gnu_inline)) gnu_inline_nonstatic(void) {}
void __attribute__((noinline)) noinline_nonstatic(void) {}
void inline inline_nonstatic(void) {}
extern void inline inline_extern(void) {}
extern void inline __attribute__((always_inline)) always_inline_extern(void) {}
extern void inline __attribute__((__gnu_inline__)) gnu_inline_extern(void) {}
extern void inline __attribute__((gnu_inline, always_inline)) always_inline_gnu_inline_extern(void) {}

void ensure_use(void) {
    always_inline_static();
    always_inline_nonstatic();
    gnu_inline_nonstatic();
    inline_static();
    noinline_static();
    inline_nonstatic();
    gnu_inline_static();
    inline_extern();
    always_inline_extern();
    gnu_inline_extern();
    always_inline_gnu_inline_extern();
}
