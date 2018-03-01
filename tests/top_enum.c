enum E {
        A = 0,
        B = 1,
};

static enum E e = 1; // We can't handle this implicit cast currently

void entry(const unsigned sz, int buffer[const]) { buffer[0] = e; }
