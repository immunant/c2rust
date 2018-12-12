#include <stdlib.h>

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

// FIXME:
void ops_three_byte_date(three_byte_date* tbd) {
    unsigned char one = 1;
    unsigned char two = 2;

    // These should not add casts to rhs
    tbd->day %= two; // 1
    tbd->day += one; // 2
    tbd->day *= two; // 4
    tbd->day -= one; // 3
    tbd->day /= two; // 1
    tbd->day ^= one; // 1
    tbd->day >>= two; // 4
    tbd->day <<= one; // 2
    tbd->day &= one; // 1
    tbd->day |= two; // 3

    // These should add casts to rhs

}
