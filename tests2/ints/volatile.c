
// Remark: This test case isn't super useful as we do not really check anything about the order of
//         the reads and writes in the generated assembly.

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
}


