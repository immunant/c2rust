static int id(int i) { return i;}
static int add(int *p, int i, int r) { *p += i; return r;}

void entry3(const unsigned sz, int buf[const])
{
        buf[0] = id(0) ?: id(1);
        buf[1] = id(2) ?: id(3);

        (void) (add(buf+2, 2, 0) ?: add(buf+3, 3, 0));
        (void) (add(buf+4, 4, 1) ?: add(buf+5, 5, 0));
}
