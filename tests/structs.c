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
}
