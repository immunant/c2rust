#include <stdatomic.h>

_Atomic(int) c11_atomics(_Atomic(int) x) {
    __c11_atomic_init(&x, 0);
    __c11_atomic_store(&x, 1, __ATOMIC_SEQ_CST);
    __c11_atomic_load(&x, __ATOMIC_SEQ_CST);
    __c11_atomic_fetch_add(&x, 2, __ATOMIC_SEQ_CST);
    __c11_atomic_fetch_sub(&x, 1, __ATOMIC_SEQ_CST);
    __c11_atomic_fetch_and(&x, 0xF, __ATOMIC_SEQ_CST);
    __c11_atomic_fetch_or(&x, 0x10, __ATOMIC_SEQ_CST);
    __c11_atomic_fetch_nand(&x, 0xFF, __ATOMIC_SEQ_CST);
    __c11_atomic_exchange(&x, 42, __ATOMIC_SEQ_CST);

    int expected = 42;
    int desired = 100;
    __c11_atomic_compare_exchange_strong(&x, &expected, desired, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    expected = 100;
    desired = 200;
    __c11_atomic_compare_exchange_weak(&x, &expected, desired, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);

    return x;
}
