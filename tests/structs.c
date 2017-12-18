struct int_pair_s {
        int x, y;
};

typedef struct int_pair_s int_pair;

struct has_padding {
    int x;
    int:1;
    int y;
};

typedef struct { int a, b; };


void entry (const unsigned sz, int buf[const]) {



}
