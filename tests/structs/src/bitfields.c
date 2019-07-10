#include <stdlib.h>
#include <stdint.h>

typedef unsigned char uchar;
typedef unsigned short ushort;

// *** Dumping AST Record Layout
//          0 | struct three_byte_date
//      0:0-4 |   unsigned char d
//      0:5-8 |   unsigned char m
//     1:1-15 |   unsigned short y
//            | [sizeof=3, align=1]
typedef struct {
    unsigned char day: 5;
    unsigned char month: 4;
    unsigned short year: 15;
} __attribute((packed)) three_byte_date;

three_byte_date zeroed_three_byte_date(void) {
    three_byte_date tbd = {0, 0, 0};

    return tbd;
}

size_t size_of_three_byte_date() {
    return sizeof(three_byte_date);
}

unsigned char compare_three_byte_date(three_byte_date const* tbd, uchar d, uchar m, ushort y) {
    if (tbd->day != d) {
        return 1;
    }

    if (tbd->month != m) {
        return 2;
    }

    if (tbd->year != y) {
        return 3;
    }

    return 0;
}

void write_three_byte_date(three_byte_date* tbd, uchar d, uchar m, ushort y) {
    tbd->day = d;
    tbd->month = m;
    tbd->year = y;

}

// *** Dumping AST Record Layout
//          0 | struct padded_bitfield
//      0:0-6 |   long x
//        2:- |   short
//      2:0-9 |   unsigned long z
//            | [sizeof=8, align=8]
typedef struct {
    signed long x: 7;
    signed short: 0;
    unsigned long z: 10;
} padded_bitfield;

padded_bitfield zeroed_padded_bitfield(void) {
    padded_bitfield pb = {0, 0};

    return pb;
}

size_t size_of_padded_bitfield() {
    return sizeof(padded_bitfield);
}

void ops_padded_bitfield(padded_bitfield* bf) {
    // These should not add casts to rhs
    // (shifts seem to add them for some reason)
    bf->x %= 2L;
    bf->x += 1L;
    bf->x *= 2L;
    bf->x -= 1L;
    bf->x /= 2L;
    bf->x ^= 1L;
    bf->x >>= 2L;
    bf->x <<= 1L;
    bf->x &= 1L;
    bf->x |= 2L;
    bf->x++;
    bf->x--;

    // These should add casts to rhs
    bf->x %= 2;
    bf->x += 1;
    bf->x *= 2;
    bf->x -= 1;
    bf->x /= 2;
    bf->x ^= 1;
    bf->x >>= 2;
    bf->x <<= 1;
    bf->x &= 1;
    bf->x |= 2;
}

padded_bitfield ops_padded_bitfield_init(void) {
    padded_bitfield bf = {13, 0};
    // These should not add casts to rhs
    // (shifts seem to add them for some reason)
    bf.x %= 2L;
    bf.x += 1L;
    bf.x *= 2L;
    bf.x -= 1L;
    bf.x /= 2L;
    bf.x ^= 1L;
    bf.x >>= 2L;
    bf.x <<= 1L;
    bf.x &= 1L;
    bf.x |= 2L;
    bf.x++;
    bf.x--;

    // These should add casts to rhs
    bf.x %= 2;
    bf.x += 1;
    bf.x *= 2;
    bf.x -= 1;
    bf.x /= 2;
    bf.x ^= 1;
    bf.x >>= 2;
    bf.x <<= 1;
    bf.x &= 1;
    bf.x |= 2;

    return bf;
}

three_byte_date static_date = {13, 12, 2018};

// *** Dumping AST Record Layout
//          0 | struct mixed_bitfields
//      0:0-9 |   unsigned long x
//          8 |   double y
//            | [sizeof=16, align=8]
typedef struct {
    unsigned long x: 10;
    double y;
} mixed_bitfields;

size_t size_of_mixed_bitfields(void) {
    return sizeof(mixed_bitfields);
}

// Initializes an array of bitfield structs and returns a
// pointer to the last one's y (non bitfield)
double* init_bitfield_array(mixed_bitfields* bfs, size_t size) {
    for (size_t i = 0; i < size; i++) {
        bfs[i].x = i;
        bfs[i].y = (double) i * 2.2;
    }

    return &bfs[size - 1].y;
}

mixed_bitfields zeroed_mixed_bitfields(void) {
    mixed_bitfields mb = {0, 0.0};

    return mb;
}

struct from_csmith {
    signed f0 : 27;
    unsigned f1 : 4;
    unsigned f2 : 22;
    const unsigned f3 : 10;
    unsigned f4 : 17;
    uint32_t  f5;
    unsigned : 0;
    signed f6 : 25;
};

size_t size_of_from_csmith(void) {
    return sizeof(struct from_csmith);
}

struct from_csmith init_from_csmith(void) {
    struct from_csmith fc = {1, 2, 3, 4, 5, 6, 7};

    return fc;
}

struct S {
    int a: 8;
} s;

struct S *get_bf_ptr(void) { return &s; }

void modify_bf_ptr(void) {
    get_bf_ptr()->a = 1;
    get_bf_ptr()->a += 1;
}

struct two_eight_bits {
    int a: 8;
    int b: 8;
};

size_t size_of_two_eight_bits(void) {
    return sizeof(struct two_eight_bits);
}

struct two_eight_bits two_eight_bits_init(void) {
    struct two_eight_bits teb = {0, 0};

    return teb;
}

unsigned char ma_results[17] = {0};

void multiple_assignments(void) {
    unsigned char a = 3, b = 2;
    three_byte_date tbd = {0, 0, 0};

    tbd.day = a = 4;

    ma_results[0] = tbd.day;
    ma_results[1] = a;

    tbd.month = tbd.day = b;

    ma_results[2] = tbd.day;
    ma_results[3] = tbd.month;

    tbd.month = tbd.day = tbd.month = tbd.year = 8;

    ma_results[4] = tbd.day;
    ma_results[5] = tbd.month;
    ma_results[6] = tbd.year;

    tbd.month = 9;
    a = b = tbd.month;

    ma_results[7] = a;
    ma_results[8] = b;

    b = tbd.day = tbd.month;

    ma_results[9] = b;
    ma_results[10] = tbd.day;

    a = tbd.day = 5;

    ma_results[11] = tbd.day;
    ma_results[12] = a;

    tbd.year = a = b;

    ma_results[13] = tbd.year;
    ma_results[14] = a;

    tbd.day += tbd.month += 1;

    ma_results[15] = tbd.day;
    ma_results[16] = tbd.month;
}

struct renamer_required {
    int a;
    unsigned int use: 1;
};

void use_renamed_field(void) {
    struct renamer_required rr;

    rr.use = 0;
}
