// Should fail

enum E {
        A = 0,
        B = 1,
};

void entry(const unsigned buffer_size, int buffer[const])

{
        int i = 0;
        if (A) { buffer[i++] = 1; }
        if (B) { buffer[i++] = 1; }
        enum E x;
        if (x) { buffer[i++] = 1; }
}
