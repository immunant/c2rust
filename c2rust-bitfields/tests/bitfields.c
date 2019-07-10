#include <stdio.h>

typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned long ulong;

// On my x86_64 machine:
// *** Dumping AST Record Layout
//         0 | struct compact_date
//     0:0-4 |   unsigned char d
//     1:0-3 |   unsigned char m
//         2 |   unsigned short y
//           | [sizeof=4, align=2]
typedef struct {
    unsigned char d: 5;
    unsigned char m: 4;
    unsigned short y;
} compact_date;

unsigned int check_compact_date(compact_date const* date, uchar d, uchar m, ushort y) {
    if (date->d != d) {
        printf("Different day: %u!\n", date->d);

        return 2;
    }

    if (date->m != m) {
        printf("Different month: %u!\n", date->m);

        return 3;
    }

    if (date->y != y) {
        printf("Different year: %hu!\n", date->y);

        return 4;
    }

    return 1;
}

void assign_compact_date_day(compact_date* date, uchar day) {
    date->d = day;
}


// *** Dumping AST Record Layout
//         0 | struct overlapping_byte_date
//     0:0-4 |   unsigned long d
//     0:5-8 |   unsigned short m
//         2 |   unsigned short y
//           | [sizeof=8, align=8]
typedef struct {
    unsigned long d: 5;
    unsigned short m: 4;
    unsigned short y;
} overlapping_byte_date;

unsigned int check_overlapping_byte_date(overlapping_byte_date const* date, ulong d, ushort m, ushort y) {
    if (date->d != d) {
        printf("Different day: %u!\n", date->d);

        return 2;
    }

    if (date->m != m) {
        printf("Different month: %u!\n", date->m);

        return 3;
    }

    if (date->y != y) {
        printf("Different year: %hu!\n", date->y);

        return 4;
    }

    return 1;
}

// *** Dumping AST Record Layout
//          0 | struct test
//          0 |   double z
//      8:0-4 |   unsigned short x
//       10:- |   unsigned short
//     10:0-8 |   unsigned short y
//            | [sizeof=16, align=8]
typedef struct {
   double z;
   unsigned short x: 5;
   unsigned short: 0;
   unsigned short y: 9;
} unnamed_bitfield;

unsigned int check_unnamed_bitfield(unnamed_bitfield const* bf, ushort x, ushort y, double z) {
    if (bf->x != x) {
        return 2;
    }

    if (bf->y != y) {
        return 3;
    }

    if (bf->z != z) {
        return 4;
    }

    return 1;
}

// *** Dumping AST Record Layout
//          0 | struct signed_bitfields
//      0:0-3 |   short x
//      0:4-8 |   unsigned short y
//      1:1-5 |   short z
//            | [sizeof=2, align=2]
typedef struct {
    signed short x: 4;
    unsigned short y: 5;
    signed short z: 5;
} signed_bitfields;

unsigned int check_signed_bitfields(signed_bitfields const* bf, signed short x, unsigned short y, signed short z) {
    if (bf->x != x) {
        return 2;
    }

    if (bf->y != y) {
        return 3;
    }

    if (bf->z != z) {
        printf("Found %hd expected %hd", bf->z, z);
        return 4;
    }

    return 1;
}

void assign_signed_bitfields(signed_bitfields* bf, signed short x, ushort y, signed short z) {
    bf->x = x;
    bf->y = y;
    bf->z = z;
}

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
} __attribute__((packed)) three_byte_date;

unsigned int check_three_byte_date(three_byte_date const* bf, uchar day, uchar month, ushort year) {
    if (bf->day != day) {
        return 2;
    }

    if (bf->month != month) {
        return 3;
    }

    if (bf->year != year) {
        return 4;
    }

    return 1;
}

void assign_three_byte_date(three_byte_date* bf, uchar day, uchar month, ushort year) {
    bf->day = day;
    bf->month = month;
    bf->year = year;
}
