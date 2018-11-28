#include <stdio.h>

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

unsigned int check_compact_date(compact_date const* date) {
    if (date->d != 31) {
        printf("Different day: %u!\n", date->d);

        return 2;
    }

    if (date->m != 12) {
        printf("Different month: %u!\n", date->m);

        return 3;
    }

    if (date->y != 2014) {
        printf("Different year: %hu!\n", date->y);

        return 4;
    }

    return 1;
}

void assign_compact_date_day(compact_date* date, unsigned char day) {
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

unsigned int check_overlapping_byte_date(overlapping_byte_date const* date, unsigned long d, unsigned short m, unsigned short y) {
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
