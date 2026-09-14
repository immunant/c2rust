typedef int (*callback)(int);

void atomic_function_pointers(callback desired) {
    callback ptr = 0;
    callback expected = 0;
    callback result;

    __atomic_store_n(&ptr, desired, __ATOMIC_RELEASE);
    result = __atomic_load_n(&ptr, __ATOMIC_ACQUIRE);
    result = __atomic_exchange_n(&ptr, (callback)0, __ATOMIC_ACQ_REL);
    __atomic_compare_exchange_n(&ptr, &expected, desired, 0,
                                __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);

    __atomic_store(&ptr, &desired, __ATOMIC_RELEASE);
    __atomic_load(&ptr, &result, __ATOMIC_ACQUIRE);
    __atomic_exchange(&ptr, &desired, &result, __ATOMIC_ACQ_REL);
    __atomic_compare_exchange(&ptr, &expected, &desired, 0,
                              __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}

void c11_atomic_function_pointers(callback desired) {
    _Atomic(callback) ptr = (callback)0;
    callback expected = 0;
    callback result;

    __c11_atomic_init(&ptr, (callback)0);
    __c11_atomic_store(&ptr, desired, __ATOMIC_RELEASE);
    result = __c11_atomic_load(&ptr, __ATOMIC_ACQUIRE);
    result = __c11_atomic_exchange(&ptr, (callback)0, __ATOMIC_ACQ_REL);
    __c11_atomic_compare_exchange_strong(&ptr, &expected, desired,
                                        __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    __c11_atomic_compare_exchange_weak(&ptr, &expected, (callback)0,
                                      __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}

void sync_function_pointers(callback desired) {
    callback ptr = 0;
    callback result;

    result = __sync_lock_test_and_set(&ptr, desired);
    result = __sync_val_compare_and_swap(&ptr, desired, (callback)0);
    __sync_bool_compare_and_swap(&ptr, (callback)0, desired);
    __sync_lock_release(&ptr);
}
