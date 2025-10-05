// record (struct/union) declaration

struct AnonEnumInStruct {
    enum {
        VALUE1,
        VALUE2
    };
};

struct AnonStructInStruct {
    struct {
        int some_number;
    };
};

struct NestedStructInStruct {
    struct InsideStruct {
        int yup;
    };
};

union AnonEnumInUnion {
    enum {
        VALUE3,
        VALUE4
    };
    int a;
};

union AnonStructInUnion {
    struct {
        int some_number;
    };
    int a;
};

union NestedStructInUnion {
    struct InsideUnion {
        int yup;
    };
    int a;
};


struct IntArray {
    int len;
    int data[] __attribute__((counted_by(len)));
};

struct IntArray2 {
    int len;
    int *data __attribute__((counted_by_or_null(len)));
};

struct RawBuffer {
    int size;
    void *ptr __attribute__((sized_by(size)));
};

struct RawBuffer2 {
    int size;
    void *ptr __attribute__((sized_by_or_null(size)));
};

void struct_declaration() {
    int value = VALUE2;
    struct AnonEnumInStruct a;
    struct AnonStructInStruct b;
    b.some_number = 7;
    struct NestedStructInStruct c;
    struct InsideStruct d;

    struct IntArray arr;
    struct IntArray2 arr2;
    struct RawBuffer buf;
    struct RawBuffer2 buf2;
}

void union_declaration() {
    int value = VALUE4;
    union AnonEnumInUnion a;
    union AnonStructInUnion b;
    b.some_number = 99;
    union NestedStructInUnion c;
    struct InsideUnion d;
}
