void atomics_entry(const unsigned buffer_size, int buffer[const])
{
    int i = 0, x = 34;
    buffer[i++] = __sync_fetch_and_add(&x, 55);    buffer[i++] = x;
    buffer[i++] = __sync_fetch_and_sub(&x, 17);    buffer[i++] = x;
    buffer[i++] = __sync_fetch_and_or(&x, 128);    buffer[i++] = x;
    buffer[i++] = __sync_fetch_and_xor(&x, 0xA5);  buffer[i++] = x;
    buffer[i++] = __sync_fetch_and_and(&x, 0xAA);  buffer[i++] = x;
    buffer[i++] = __sync_fetch_and_nand(&x, 0xA0); buffer[i++] = x;

    buffer[i++] = __sync_add_and_fetch(&x, 55);    buffer[i++] = x;
    buffer[i++] = __sync_sub_and_fetch(&x, 17);    buffer[i++] = x;
    buffer[i++] = __sync_or_and_fetch(&x, 128);    buffer[i++] = x;
    buffer[i++] = __sync_xor_and_fetch(&x, 0xA5);  buffer[i++] = x;
    buffer[i++] = __sync_and_and_fetch(&x, 0xAA);  buffer[i++] = x;
    buffer[i++] = __sync_nand_and_fetch(&x, 0xA0); buffer[i++] = x;

    x &= 0xFF;
    buffer[i++] = x;
    for (int x = 0; x < 256; x++) {
        buffer[i++] = __sync_val_compare_and_swap(&x, i, 137);
        buffer[i++] = x;
    }
    for (int x = 130; x < 140; x++) {
        buffer[i++] = __sync_bool_compare_and_swap(&x, i, 87);
        buffer[i++] = x;
    }

    buffer[i++] = __sync_lock_test_and_set(&x, 33);
    buffer[i++] = x;
    __sync_lock_release(&x);
    buffer[i++] = x;
}

void new_atomics(const unsigned buffer_size, int buffer[const])
{
    int i = 0, x = 34;
    buffer[i++] = __atomic_fetch_add(&x, 55, __ATOMIC_RELAXED);    buffer[i++] = x;
    buffer[i++] = __atomic_fetch_sub(&x, 17, __ATOMIC_RELAXED);    buffer[i++] = x;
    buffer[i++] = __atomic_fetch_or(&x, 128, __ATOMIC_RELAXED);    buffer[i++] = x;
    buffer[i++] = __atomic_fetch_xor(&x, 0xA5, __ATOMIC_RELAXED);  buffer[i++] = x;
    buffer[i++] = __atomic_fetch_and(&x, 0xAA, __ATOMIC_RELAXED);  buffer[i++] = x;
    buffer[i++] = __atomic_fetch_nand(&x, 0xA0, __ATOMIC_RELAXED); buffer[i++] = x;

    buffer[i++] = __atomic_add_fetch(&x, 55, __ATOMIC_RELAXED);    buffer[i++] = x;
    buffer[i++] = __atomic_sub_fetch(&x, 17, __ATOMIC_RELAXED);    buffer[i++] = x;
    buffer[i++] = __atomic_or_fetch(&x, 128, __ATOMIC_RELAXED);    buffer[i++] = x;
    buffer[i++] = __atomic_xor_fetch(&x, 0xA5, __ATOMIC_RELAXED);  buffer[i++] = x;
    buffer[i++] = __atomic_and_fetch(&x, 0xAA, __ATOMIC_RELAXED);  buffer[i++] = x;
    buffer[i++] = __atomic_nand_fetch(&x, 0xA0, __ATOMIC_RELAXED); buffer[i++] = x;

    x &= 0xFF;
    buffer[i++] = x;
    for (int x = 0; x < 256; x++) {
        buffer[i++] = __atomic_compare_exchange_n(&x, &i, 137, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
        buffer[i++] = x;
    }
    int val = 89;
    for (int x = 130; x < 140; x++) {
        buffer[i++] = __atomic_compare_exchange(&x, &i, &val, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
        buffer[i++] = x;
    }

    buffer[i++] = __atomic_exchange_n(&x, 33, __ATOMIC_RELAXED);
    buffer[i++] = x;
    __atomic_store_n(&x, 0, __ATOMIC_RELAXED);
    buffer[i++] = x;
}
