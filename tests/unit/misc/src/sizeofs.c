#include <assert.h>

struct a_struct {
        char a;
        int b;
};

union a_union {
        char a;
        int b;
};

typedef unsigned long long ull_typedef;

void sizeofs(const unsigned n, int * const buffer) {

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

        /* these should have 8 byte __alignof__ even on 32-bit systems, because
         * __align gives "preferred" alignment for these types, not the ABI
         * alignment. */
        CHECK(double);
        CHECK(long long);
        CHECK(unsigned long long);
        CHECK(ull_typedef);

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

        int x1[4];
        int x2[4][5];
        int x3[3][a];
        int x4[b][4];
        int x5[a][b];
        int(*x6[3])[b];
        int(*x7)[a][b];

        a = 0;
        b = 0;

        CHECK(x1);
        CHECK(x2);
        CHECK(x3);
        CHECK(x4);
        CHECK(x5);
        CHECK(x6);
        CHECK(x7);

        assert(i == n);
}
