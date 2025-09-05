// record (struct/union) declaration

struct AnonEnumInStruct
{
    enum
    {
        VALUE1,
        VALUE2
    };
};

struct AnonStructInStruct
{
    struct
    {
        int some_number;
    };
};

struct NestedStructInStruct
{
    struct StructInsider
    {
        int yup;
    };
};


union AnonEnumInUnion
{
    enum
    {
        VALUE3,
        VALUE4
    };
    int a;
};

union AnonStructInUnion
{
    struct
    {
        int some_number;
    };
    int a;
};

union NestedStructInUnion
{
    struct UnionInsider
    {
        int yup;
    };
    int a;
};

void struct_declaration()
{
    int value = VALUE2;
    struct AnonEnumInStruct a;
    struct AnonStructInStruct b;
    b.some_number = 7;
    struct NestedStructInStruct c;
    struct StructInsider d;
}

void union_declaration()
{
    int value = VALUE4;
    union AnonEnumInUnion a;
    union AnonStructInUnion b;
    b.some_number = 99;
    union NestedStructInUnion c;
    struct UnionInsider d;
}