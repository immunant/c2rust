#include <stdalign.h>
#include <stddef.h>
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
DEFINE_TEST_STRUCT(S12) __attribute__((packed, aligned(1)));
DEFINE_TEST_STRUCT(S13) __attribute__((packed, aligned(2)));
DEFINE_TEST_STRUCT(S14) __attribute__((packed, aligned(4)));
DEFINE_TEST_STRUCT(S15) __attribute__((packed, aligned(8)));
DEFINE_TEST_STRUCT(S16) __attribute__((packed, aligned(16)));
#undef DEFINE_TEST_STRUCT

#define DEFINE_TEST_STRUCT(x, y) struct x { char c; struct y s; }
DEFINE_TEST_STRUCT(S17, S7);
DEFINE_TEST_STRUCT(S18, S8);
DEFINE_TEST_STRUCT(S19, S9);
DEFINE_TEST_STRUCT(S20, S10);
DEFINE_TEST_STRUCT(S21, S11);
DEFINE_TEST_STRUCT(S22, S12);
DEFINE_TEST_STRUCT(S23, S13);
DEFINE_TEST_STRUCT(S24, S14);
DEFINE_TEST_STRUCT(S25, S15);
DEFINE_TEST_STRUCT(S26, S16);
DEFINE_TEST_STRUCT(S27, S7) __attribute__((packed));
DEFINE_TEST_STRUCT(S28, S8) __attribute__((packed));
DEFINE_TEST_STRUCT(S29, S9) __attribute__((packed));
DEFINE_TEST_STRUCT(S30, S10) __attribute__((packed));
DEFINE_TEST_STRUCT(S31, S11) __attribute__((packed));
DEFINE_TEST_STRUCT(S32, S12) __attribute__((packed));
DEFINE_TEST_STRUCT(S33, S13) __attribute__((packed));
DEFINE_TEST_STRUCT(S34, S14) __attribute__((packed));
DEFINE_TEST_STRUCT(S35, S15) __attribute__((packed));
DEFINE_TEST_STRUCT(S36, S16) __attribute__((packed));
#undef DEFINE_TEST_STRUCT

void alignment_entry(const unsigned sz, int buf[const]) {
    int i = 0;

#define CHECK_TEST_STRUCT(S) \
    do { \
        struct S s = { .c = 42, .s = 1337, .ll = 65537 }; \
        buf[i++] = sizeof(s); \
        buf[i++] = alignof(s); \
        buf[i++] = s.c; \
        buf[i++] = s.s; \
        buf[i++] = s.ll; \
        buf[i++] = offsetof(struct S, c); \
        buf[i++] = offsetof(struct S, s); \
        buf[i++] = offsetof(struct S, ll); \
        struct S sz = { }; \
        buf[i++] = sz.c; \
        buf[i++] = sz.s; \
        buf[i++] = sz.ll; \
    } while (0)
    CHECK_TEST_STRUCT(S1);
    CHECK_TEST_STRUCT(S2);
    CHECK_TEST_STRUCT(S3);
    CHECK_TEST_STRUCT(S4);
    CHECK_TEST_STRUCT(S5);
    CHECK_TEST_STRUCT(S6);
    CHECK_TEST_STRUCT(S7);
    CHECK_TEST_STRUCT(S8);
    CHECK_TEST_STRUCT(S9);
    CHECK_TEST_STRUCT(S10);
    CHECK_TEST_STRUCT(S11);
    CHECK_TEST_STRUCT(S12);
    CHECK_TEST_STRUCT(S13);
    CHECK_TEST_STRUCT(S14);
    CHECK_TEST_STRUCT(S15);
    CHECK_TEST_STRUCT(S16);
#undef CHECK_TEST_STRUCT

#define CHECK_TEST_STRUCT(S) \
    do { \
        struct S s = { }; \
        buf[i++] = sizeof(s); \
        buf[i++] = alignof(s); \
        buf[i++] = offsetof(struct S, s.c); \
        buf[i++] = offsetof(struct S, s.s); \
        buf[i++] = offsetof(struct S, s.ll); \
    } while (0)
    CHECK_TEST_STRUCT(S17);
    CHECK_TEST_STRUCT(S18);
    CHECK_TEST_STRUCT(S19);
    CHECK_TEST_STRUCT(S20);
    CHECK_TEST_STRUCT(S21);
    CHECK_TEST_STRUCT(S22);
    CHECK_TEST_STRUCT(S23);
    CHECK_TEST_STRUCT(S24);
    CHECK_TEST_STRUCT(S25);
    CHECK_TEST_STRUCT(S26);
    CHECK_TEST_STRUCT(S27);
    CHECK_TEST_STRUCT(S28);
    CHECK_TEST_STRUCT(S29);
    CHECK_TEST_STRUCT(S30);
    CHECK_TEST_STRUCT(S31);
    CHECK_TEST_STRUCT(S32);
    CHECK_TEST_STRUCT(S33);
    CHECK_TEST_STRUCT(S34);
    CHECK_TEST_STRUCT(S35);
    CHECK_TEST_STRUCT(S36);
#undef CHECK_TEST_STRUCT
}
