#include <stdio.h>
#include <stdint.h>

uint64_t foo(void) {
    return 0x123457890ABCDEFULL;
}

uint64_t id(uint64_t x) {
    return x;
}

struct Foo {
    uint64_t n1, n2;
};

uint64_t my_Foo_hash(struct Foo x) {
    return (128 + x.n1) * 256 + (x.n2 + 128);
}

uint64_t fibo(uint64_t n, const uint64_t *p, const uint64_t *q, struct Foo foo) {
#if 0
    printf("fibo call:%ld %p %p\n", n, p, q);
#endif
    if (n <= 1) {
        return 1;
    } else {
        struct Foo foo1 = { foo.n2, n - 3 };
        struct Foo foo2 = { n - 3, n - 4 };
        return fibo(foo.n1, p, q, foo1) + fibo(foo.n2, q, p, foo2);
    }
}

int main() {
#if 0
    printf("Profiling test!!!\n");
    fibo(4);
#endif
    for (size_t i = 0; i < 5; i++) {
        struct Foo foo = { i - 1, i - 2 };
        fibo(i, &i, NULL, foo);
#if 0
        printf("fibo(%zd)=%llu\n", i, fibo(i));
#endif
    }
    return 0;
}
