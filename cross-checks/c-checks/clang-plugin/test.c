#include <stdio.h>
#include <stdint.h>

uint64_t foo(void) {
    return 0x123457890ABCDEFULL;
}

uint64_t id(uint64_t x) {
    return x;
}

struct Foo {
    uint64_t a, b;
};

uint64_t fibo(uint64_t n, const uint64_t *p, struct Foo foo) {
    if (n <= 1) {
        return 1;
    } else {
        return fibo(n - 1, p, foo) + fibo(n - 2, p, foo);
    }
}

int main() {
#if 0
    printf("Profiling test!!!\n");
    fibo(4);
#endif
    for (size_t i = 0; i < 5; i++) {
        struct Foo foo = { i, i };
        fibo(i, &i, foo);
#if 0
        printf("fibo(%zd)=%llu\n", i, fibo(i));
#endif
    }
    return 0;
}
