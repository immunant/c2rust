typedef int (*callback)(int);

static int increment(int x) { return x + 1; }
static int decrement(int x) { return x - 1; }

int atomic_function_pointers(void) {
    callback ptr = 0;
    callback expected = 0;
    callback desired = increment;
    callback result = 0;

    __atomic_store_n(&ptr, increment, __ATOMIC_RELEASE);
    result = __atomic_load_n(&ptr, __ATOMIC_ACQUIRE);
    if (!result || result(10) != 11) return 1;
    result = __atomic_exchange_n(&ptr, decrement, __ATOMIC_ACQ_REL);
    if (result != increment) return 2;
    if (__atomic_compare_exchange_n(&ptr, &expected, increment, 0,
                                    __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) return 3;
    if (expected != decrement) return 4;
    if (!__atomic_compare_exchange_n(&ptr, &expected, (callback)0, 0,
                                     __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) return 5;
    if (__atomic_load_n(&ptr, __ATOMIC_RELAXED)) return 6;

    __atomic_store(&ptr, &desired, __ATOMIC_RELEASE);
    __atomic_load(&ptr, &result, __ATOMIC_ACQUIRE);
    if (result != increment) return 7;
    desired = decrement;
    __atomic_exchange(&ptr, &desired, &result, __ATOMIC_ACQ_REL);
    if (result != increment) return 8;
    expected = increment;
    desired = 0;
    if (__atomic_compare_exchange(&ptr, &expected, &desired, 0,
                                  __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) return 9;
    if (expected != decrement) return 10;
    if (!__atomic_compare_exchange(&ptr, &expected, &desired, 0,
                                   __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) return 11;
    __atomic_load(&ptr, &result, __ATOMIC_RELAXED);
    if (result) return 12;

    return 0;
}
