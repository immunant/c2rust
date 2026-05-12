#define inc(x) (_Generic((x), \
    float: (x) + 1.0f,        \
    double: (x) + 1.0,        \
    default: (x) + 1))

struct s { int x; };
struct t { int y; };
#define cast(p) (_Generic((p), \
    const struct s *: (const struct t *)(p), \
    struct s *:       (struct t *)(p)))

#define unqual_typeof(x) typeof(_Generic((x), \
    int: (int)0, const int: (int)0, default: (x)))

void foo() {
    int x = inc(42);
    double y = inc(42.0);
    float z = inc(42.0f);

    int n = 0;
    _Generic((n++), int: n, default: n);  // n must end up 0, not 1

    // No default
    int m = 0;
    n = _Generic((m), int: m);

    struct s s;
    struct s *p1 = &s;
    const struct s *p2 = &s;
    struct t *t1 = cast(p1);
    const struct t *t2 = cast(p2);

    const int cx = n;
    unqual_typeof(x) ut_y = cx;
}
