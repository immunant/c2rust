#include <stdint.h>

// Define __c2rust_hash_T functions for all the fixed-size types
#define DEFINE_FIXED_HASH(short_ty, val_ty, xor_const)      \
    static uint64_t __c2rust_hash_ ## short_ty (val_ty x) { \
        return (xor_const) ^ (uint64_t) x;                  \
    }
DEFINE_FIXED_HASH(u8,  uint8_t,  0x0000000000000000ULL)
DEFINE_FIXED_HASH(u16, uint16_t, 0x5a5a5a5a5a5a5a5aULL)
DEFINE_FIXED_HASH(u32, uint32_t, 0xb4b4b4b4b4b4b4b4ULL)
DEFINE_FIXED_HASH(u64, uint64_t, 0x0f0f0f0f0f0f0f0eULL)
DEFINE_FIXED_HASH(i8,   int8_t,  0xc3c3c3c3c3c3c3c2ULL)
DEFINE_FIXED_HASH(i16,  int16_t, 0x1e1e1e1e1e1e1e1cULL)
DEFINE_FIXED_HASH(i32,  int32_t, 0x7878787878787876ULL)
DEFINE_FIXED_HASH(i64,  int64_t, 0xd2d2d2d2d2d2d2d0ULL)

// Now define __c2rust_hash_T functions for primitive C types
// as aliases to the fixed-size functions defined above
#define STRINGIFY(x)  #x
#define DEFINE_CTYPE_HASH(c_ty_name, c_ty, sign, width) \
    uint64_t __c2rust_hash_ ## c_ty_name (c_ty x)       \
    __attribute__((alias("__c2rust_hash_" sign STRINGIFY(width))));
DEFINE_CTYPE_HASH(uchar,  unsigned char,      "u", __SCHAR_WIDTH__);
DEFINE_CTYPE_HASH(ushort, unsigned short,     "u", __SHRT_WIDTH__);
DEFINE_CTYPE_HASH(uint,   unsigned int,       "u", __INT_WIDTH__);
DEFINE_CTYPE_HASH(ulong,  unsigned long,      "u", __LONG_WIDTH__);
DEFINE_CTYPE_HASH(ullong, unsigned long long, "u", __LONG_LONG_WIDTH__);
DEFINE_CTYPE_HASH(schar,  signed char,        "i", __SCHAR_WIDTH__);
DEFINE_CTYPE_HASH(short,  short,              "i", __SHRT_WIDTH__);
DEFINE_CTYPE_HASH(int,    int,                "i", __INT_WIDTH__);
DEFINE_CTYPE_HASH(long,   long,               "i", __LONG_WIDTH__);
DEFINE_CTYPE_HASH(llong,  unsigned long long, "i", __LONG_LONG_WIDTH__);

// TODO: implement more types, e.g., bool, char, double, float
