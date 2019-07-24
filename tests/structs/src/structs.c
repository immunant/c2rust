#include <stdalign.h>
#include <stdio.h>

struct int_pair_s {
    int x, y;
};

typedef struct int_pair_s int_pair;

struct has_padding {
    int x;
    int:1;
    int y;
};

typedef struct { int a, b; } anon;


void entry (const unsigned sz, int buf[const]) {
    // anonymous and nested anonymous structs
    struct {
            struct { int x, y; } n;
            int z;
    } local = { .n.x = 1, .n.y = 2, .z = 3}; // initializers

    int i = 0;

    buf[i++] = local.n.x;
    buf[i++] = local.n.y;
    buf[i++] = local.z;

    struct int_pair_s pair1, pair2;
    pair1.x = 10;
    pair1.y = 20;

    pair2 = pair1; // struct assignment

    buf[i++] = pair2.x;
    buf[i++] = pair2.y;


    struct int_pair_s uninit = {0};

    buf[i++] = uninit.x;
    buf[i++] = uninit.y;

    struct { struct { int x; }; struct { int y; }; } z = {0};
    buf[i++] = z.x;
    buf[i++] = z.y;
}

typedef struct {
    int a;
    long long int b;
} __attribute__((aligned(8))) Aligned8Struct;

size_t alignment_of_aligned8_struct(void) {
    return alignof(Aligned8Struct);
}

#define DEFINE_TEST_STRUCT(x) struct x { char c; short s; long long ll; }
DEFINE_TEST_STRUCT(S1);

#pragma pack(push, 1)
DEFINE_TEST_STRUCT(S2);

#pragma pack(2)
DEFINE_TEST_STRUCT(S3);

#pragma pack(4)
DEFINE_TEST_STRUCT(S4);

#pragma pack(8)
DEFINE_TEST_STRUCT(S5);
#pragma pack(pop)

DEFINE_TEST_STRUCT(S6) __attribute__((packed));
DEFINE_TEST_STRUCT(S7) __attribute__((aligned(1)));
DEFINE_TEST_STRUCT(S8) __attribute__((aligned(2)));
DEFINE_TEST_STRUCT(S9) __attribute__((aligned(4)));
DEFINE_TEST_STRUCT(S10) __attribute__((aligned(8)));
DEFINE_TEST_STRUCT(S11) __attribute__((aligned(16)));
#undef DEFINE_TEST_STRUCT

void alignment_entry(const unsigned sz, int buf[const]) {
    int i = 0;

#define CHECK_TEST_STRUCT(s) do { buf[i++] = sizeof(s); buf[i++] = alignof(s); } while (0)
    CHECK_TEST_STRUCT(struct S1);
    CHECK_TEST_STRUCT(struct S2);
    CHECK_TEST_STRUCT(struct S3);
    CHECK_TEST_STRUCT(struct S4);
    CHECK_TEST_STRUCT(struct S5);
    CHECK_TEST_STRUCT(struct S6);
    CHECK_TEST_STRUCT(struct S7);
    CHECK_TEST_STRUCT(struct S8);
    CHECK_TEST_STRUCT(struct S9);
    CHECK_TEST_STRUCT(struct S10);
    CHECK_TEST_STRUCT(struct S11);
#undef CHECK_TEST_STRUCT
}
