void entry2(const unsigned sz, int buf[const]) {
        for (int *cursor = buf; cursor < buf + sz; cursor += 1) {
                *cursor = 1;
        }

        for (int *cursor = buf + 10; cursor > buf; cursor -= 1) {
                *cursor = 2;
        }

        int * p = buf + 30;
        *(p - 10U) = 33;
        *(p--) = 34;
        *(p-=2) = 35;
}
