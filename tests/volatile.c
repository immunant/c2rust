
void entry(void)
{
    // direct write
    volatile int n = 0;
    int x = (n = 5);
    n += 4;

    // indirect write
    volatile int *p = &n;
    *p = 5;
    *p += 4;
}


