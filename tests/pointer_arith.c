void entry(const unsigned sz, int buf[const]) {
        for (int *cursor = buf; cursor < buf + sz; cursor += 1) {
                *cursor = 1;
        }

        for (int *cursor = buf + 10; cursor > buf; cursor -= 1) {
                *cursor = 2;
        }
}
