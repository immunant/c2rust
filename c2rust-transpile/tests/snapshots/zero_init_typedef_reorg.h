/* Zero-initializing this struct reuses the cached `STAT_T { ... }`
 * initializer for the `st` field, even though the field is spelled with the
 * struct tag. The `use STAT_T` this header module needs must therefore be
 * replayed from the imports cached alongside the initializer; the imports
 * derived from the type of the hit site only cover `Stat`. */
struct OuterHeader {
    int y;
    struct Stat st;
};

static void after_include(void) {
    struct OuterHeader o = {0};
    (void)o;
}
