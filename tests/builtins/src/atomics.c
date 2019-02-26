void atomics_entry(const unsigned buffer_size, int buffer[const])
{
    int i = 0;

    volatile int x = 34;
    __sync_fetch_and_add(&x, 55);    buffer[i++] = x;
    __sync_fetch_and_sub(&x, 17);    buffer[i++] = x;
    __sync_fetch_and_or(&x, 128);    buffer[i++] = x;
    __sync_fetch_and_xor(&x, 0xA5);  buffer[i++] = x;
    __sync_fetch_and_and(&x, 0xAA);  buffer[i++] = x;
    __sync_fetch_and_nand(&x, 0x55); buffer[i++] = x;

    __sync_add_and_fetch(&x, 55);    buffer[i++] = x;
    __sync_sub_and_fetch(&x, 17);    buffer[i++] = x;
    __sync_or_and_fetch(&x, 128);    buffer[i++] = x;
    __sync_xor_and_fetch(&x, 0xA5);  buffer[i++] = x;
    __sync_and_and_fetch(&x, 0xAA);  buffer[i++] = x;
    __sync_nand_and_fetch(&x, 0xA0); buffer[i++] = x;

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
