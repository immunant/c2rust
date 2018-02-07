// Should fail

enum e {
        A = 0,
        B = 0,
};

void entry (const unsigned int sz, int buffer[const])
{
        int x = A;
        x = B;
}
