struct a_struct {
        char a;
        int b;
};

union a_union {
        char a;
        int b;
};

void entry(const unsigned n, int * const buffer) {

        unsigned long i = 0;

#define CHECK(x) \
        buffer[i++] = sizeof(x); \
        buffer[i++] = __alignof__(x)

        CHECK(_Bool);
        CHECK(char);
        CHECK(int);
        CHECK(long);
        CHECK(unsigned);
        CHECK(unsigned long);
        CHECK(float);
        CHECK(double);

        CHECK(void*);
        CHECK(int*);
        CHECK(int(*)(int));

        CHECK(struct a_struct);
        CHECK(union a_union);

        unsigned a = 6;
        unsigned b = 7;

        CHECK(int[4]);
        CHECK(int[4][5]);
        CHECK(int[3][a]);
        CHECK(int[b][4]);
        CHECK(int[a][b]);
        CHECK(int(*)[a][b]);
        CHECK(int(*[3])[b]);
}
