typedef struct {
    struct {
        struct {
            union {
                int d;
                float e;
            } c;
        } b;
    } a;
} S1;

typedef struct {
    int a;
} S2;

S1 kS1;
S2 kS2;