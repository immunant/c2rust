#include <stdio.h>

#define eprintf(...) fprintf (stderr, __VA_ARGS__)

// On my x86_64 machine:
// *** Dumping AST Record Layout
//         0 | struct date
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
