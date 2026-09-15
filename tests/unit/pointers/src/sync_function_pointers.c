typedef int (*callback)(int);

static int increment(int x) { return x + 1; }
static int decrement(int x) { return x - 1; }

int sync_function_pointers(void) {
    callback ptr = 0;
    callback result = __sync_lock_test_and_set(&ptr, increment);
    if (result || !ptr || ptr(10) != 11) return 1;
    result = __sync_lock_test_and_set(&ptr, decrement);
    if (result != increment || ptr != decrement) return 2;

    result = __sync_val_compare_and_swap(&ptr, increment, (callback)0);
    if (result != decrement || ptr != decrement) return 3;
    result = __sync_val_compare_and_swap(&ptr, decrement, (callback)0);
    if (result != decrement || ptr) return 4;
    result = __sync_val_compare_and_swap(&ptr, (callback)0, increment);
    if (result || ptr != increment) return 5;

    if (__sync_bool_compare_and_swap(&ptr, decrement, (callback)0)) return 6;
    if (ptr != increment) return 7;
    if (!__sync_bool_compare_and_swap(&ptr, increment, (callback)0)) return 8;
    if (ptr) return 9;
    if (!__sync_bool_compare_and_swap(&ptr, (callback)0, decrement)) return 10;
    if (!ptr || ptr(10) != 9) return 11;

    __sync_lock_release(&ptr);
    if (ptr) return 12;
    return 0;
}
