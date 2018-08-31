
// Remark: This test case isn't super useful as we do not really check anything about the order of
//         the reads and writes in the generated assembly.

typedef struct some_struct {
    char buffer[10];
} some_struct;

void mutate_buffer(volatile char *dest, volatile const char *src, int size) {
    while(size-- != 0)
        *(dest++) = *(src++);
}

void entry3(const unsigned buffer_size, int buffer[])
{
    if (buffer_size < 5) { return; }

    // direct write/read
    volatile int n = 0;
    buffer[0] = (n = 5);
    n += 4;
    buffer[1] = n + 2;

    // indirect write/read
    volatile int *p = &n;
    *p = 5;
    buffer[2] = n;
    *p += 4;
    buffer[3] = *p;

    volatile signed char c = 10;
    c *= 9.9; // computation happens at a promoted type
    buffer[4] = c;

    char src[4] = "test";
    volatile some_struct s;
    mutate_buffer(s.buffer, src, 4);

    (void)s.buffer;

    buffer[5] = s.buffer[0];
    buffer[6] = s.buffer[1];
    buffer[7] = s.buffer[2];
    buffer[8] = s.buffer[3];
}


