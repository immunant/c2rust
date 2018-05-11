#include <stdint.h>
#include <stddef.h>

#define _WIDTH_HASH_FUNCTION(SIGN, WIDTH) __c2rust_hash_##SIGN##WIDTH
#define WIDTH_HASH_FUNCTION(SIGN, WIDTH)  _WIDTH_HASH_FUNCTION(SIGN, WIDTH)
#define POINTER_HASH_FUNCTION(...)        WIDTH_HASH_FUNCTION(u, __INTPTR_WIDTH__) (__VA_ARGS__)

// Define __c2rust_hash_T functions for all the fixed-size types
#define DEFINE_FIXED_HASH(short_ty, val_ty, xor_const)                    \
    static uint64_t __c2rust_hash_ ## short_ty (val_ty x, size_t depth) { \
        return (0x ## xor_const ## ULL) ^ (uint64_t) x;                   \
    }
DEFINE_FIXED_HASH(u8,  uint8_t,  0000000000000000)
DEFINE_FIXED_HASH(u16, uint16_t, 5a5a5a5a5a5a5a5a)
DEFINE_FIXED_HASH(u32, uint32_t, b4b4b4b4b4b4b4b4)
DEFINE_FIXED_HASH(u64, uint64_t, 0f0f0f0f0f0f0f0e)
DEFINE_FIXED_HASH(i8,   int8_t,  c3c3c3c3c3c3c3c2)
DEFINE_FIXED_HASH(i16,  int16_t, 1e1e1e1e1e1e1e1c)
DEFINE_FIXED_HASH(i32,  int32_t, 7878787878787876)
DEFINE_FIXED_HASH(i64,  int64_t, d2d2d2d2d2d2d2d0)

// Now define __c2rust_hash_T functions for primitive C types
// as aliases to the fixed-size functions defined above
#define _STRINGIFY(x)   #x
#define STRINGIFY(x)    _STRINGIFY(x)
#define DEFINE_CTYPE_HASH(c_ty_name, c_ty, sign, width)         \
    uint64_t __c2rust_hash_ ## c_ty_name (c_ty x, size_t depth) \
    __attribute__((alias(STRINGIFY(WIDTH_HASH_FUNCTION(sign, width)))));
DEFINE_CTYPE_HASH(uchar,  unsigned char,      u, __SCHAR_WIDTH__);
DEFINE_CTYPE_HASH(ushort, unsigned short,     u, __SHRT_WIDTH__);
DEFINE_CTYPE_HASH(uint,   unsigned int,       u, __INT_WIDTH__);
DEFINE_CTYPE_HASH(ulong,  unsigned long,      u, __LONG_WIDTH__);
DEFINE_CTYPE_HASH(ullong, unsigned long long, u, __LONG_LONG_WIDTH__);
DEFINE_CTYPE_HASH(schar,  signed char,        i, __SCHAR_WIDTH__);
DEFINE_CTYPE_HASH(short,  short,              i, __SHRT_WIDTH__);
DEFINE_CTYPE_HASH(int,    int,                i, __INT_WIDTH__);
DEFINE_CTYPE_HASH(long,   long,               i, __LONG_WIDTH__);
DEFINE_CTYPE_HASH(llong,  unsigned long long, i, __LONG_LONG_WIDTH__);
#ifdef __CHAR_UNSIGNED__
DEFINE_CTYPE_HASH(char,   char,               u, __SCHAR_WIDTH__);
#else
DEFINE_CTYPE_HASH(char,   char,               i, __SCHAR_WIDTH__);
#endif

uint64_t __c2rust_hash_bool(_Bool x, size_t depth) {
    return x ? 0x8787878787878785ULL : 0x8787878787878784ULL;
}

// TODO: implement more types, e.g., bool, char, double, float

#if __SIZEOF_FLOAT__ == 4
uint64_t __c2rust_hash_float(float x, size_t depth) {
    union {
        float f;
        uint32_t u;
    } xx = { .f = x };
    return 0x3c3c3c3c3c3c3c38ULL ^ (uint64_t) xx.u;
}
#else
#error "Unknown size for float"
#endif

#if __SIZEOF_DOUBLE__ == 8
uint64_t __c2rust_hash_double(double x, size_t depth) {
    union {
        double d;
        uint64_t u;
    } xx = { .d = x };
    return 0x9696969696969692ULL ^ (uint64_t) xx.u;
}
#else
#error "Unknown size for double"
#endif

#define LEAF_POINTER_HASH     0x726174536661654cULL // "LeafStar" in ASCII
#define LEAF_ARRAY_HASH       0x797272416661654cULL // "LeafArry" in ASCII
#define LEAF_RECORD_HASH      0x647263526661654cULL // "LeafRcrd" in ASCII
#define NULL_POINTER_HASH     0x726174536c6c754eULL // "NullStar" in ASCII
#define VOID_POINTER_HASH     0x7261745364696f56ULL // "VoidStar" in ASCII
#define FUNC_POINTER_HASH     0x72617453636e7546ULL // "FuncStar" in ASCII
#define ANY_UNION_HASH        0x6e6f696e55796e41ULL // "AnyUnion" in ASCII

_Bool __c2rust_pointer_is_invalid(void *p) {
    return p == NULL;
}

uint64_t __c2rust_hash_invalid_pointer(void *p) {
    return NULL_POINTER_HASH;
}

uint64_t __c2rust_hash_pointer_leaf() {
    return LEAF_POINTER_HASH;
}

uint64_t __c2rust_hash_array_leaf() {
    return LEAF_ARRAY_HASH;
}

uint64_t __c2rust_hash_record_leaf() {
    return LEAF_RECORD_HASH;
}

uint64_t __c2rust_hash_anyunion() {
    return ANY_UNION_HASH;
}

uint64_t __c2rust_hash_void_ptr(void *p, size_t depth) {
    if (p == NULL)
        return NULL_POINTER_HASH;
    if (depth == 0)
        return LEAF_POINTER_HASH;
    return VOID_POINTER_HASH;
}

uint64_t __c2rust_hash_function(void *f, size_t depth) {
    if (f == NULL)
        return NULL_POINTER_HASH;
    if (depth == 0)
        return LEAF_POINTER_HASH;
    return FUNC_POINTER_HASH;
}

// JodyHasher implementation
struct hasher_jodyhash_t {
    uint64_t state;
};

#define JODY_HASH_CONSTANT  0x1f3d5b79UL

unsigned int __c2rust_hasher_jodyhash_size() {
    return sizeof(struct hasher_jodyhash_t) / sizeof(char);
}

void __c2rust_hasher_jodyhash_init(char *p) {
    struct hasher_jodyhash_t *jh = (struct hasher_jodyhash_t*) p;
    jh->state = 0;
}

void __c2rust_hasher_jodyhash_update(char *p, uint64_t x) {
    struct hasher_jodyhash_t *jh = (struct hasher_jodyhash_t*) p;
    jh->state += x;
    jh->state += JODY_HASH_CONSTANT;
    jh->state = (jh->state << 14) | (jh->state >> 50);
    jh->state ^= x;
    jh->state = (jh->state << 14) | (jh->state >> 50);
    jh->state ^= JODY_HASH_CONSTANT;
    jh->state += x;
}

uint64_t __c2rust_hasher_jodyhash_finish(char *p) {
    struct hasher_jodyhash_t *jh = (struct hasher_jodyhash_t*) p;
    return jh->state;
}
