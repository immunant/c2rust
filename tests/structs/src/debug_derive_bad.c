//! derive_debug

// A struct containing a union is not debuggable
typedef struct {
    struct {
        struct {
            union {
                int d;
                float e;
            } c;
        } b;
    } a;
} StructWithUnion;

StructWithUnion kStructWithUnion;