enum E {
        A = 0,
        B = 1,
};

void entry(const unsigned buffer_size, int buffer[const]) {
        int i = 0;
        if (A) { buffer[i++] = 1; }
        if (B) { buffer[i++] = 1; }

        enum E x = A;
        if (x) { buffer[i++] = 1; }
        if (x + 1) { buffer[i++] = 1; }
        if (x - 1) { buffer[i++] = 1; }
        if (!x) { buffer[i++] = 1; }

        x = B;
        if (x) { buffer[i++] = 1; }
        if (x + 1) { buffer[i++] = 1; }
        if (x - 1) { buffer[i++] = 1; }
        if (!x) { buffer[i++] = 1; }

        // GH issue #88: Compound assignment previously tried to
        // cast int to enum (rustc error) instead of transmuting
        enum E e = A;

        e |= B;
}
