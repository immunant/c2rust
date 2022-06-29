union my_union {
    int as_int;
    char as_chars[10];
};

union my_union_flipped {
    int as_int;
    char as_chars[10];
};

union empty_union {
};

union union_with_anon_struct {
    struct {
        int a;
    };
};

union __attribute__((packed)) packed_union {
    int as_int;
    char as_chars[5];
};

void entry(const unsigned int buffer_size, int buffer[const])
{
    int i = 0;

    union my_union u1 = { .as_int = 1 };
    union my_union u2 = { 2 };
    union my_union u3 = { .as_chars = {3,4} };

    union my_union_flipped u4 = { .as_int = 5 };
    union my_union_flipped u5 = { 6 };
    union my_union_flipped u6 = { .as_chars = {7,8} };

    // Unions with anonymous structs would previously fail to init:
    union union_with_anon_struct anon;

    buffer[i++] = sizeof(union my_union);
    buffer[i++] = sizeof(union my_union_flipped);
    buffer[i++] = sizeof(union empty_union);
    buffer[i++] = sizeof(union packed_union);
    buffer[i++] = u1.as_int;
    buffer[i++] = u2.as_int;
    buffer[i++] = u3.as_chars[0];
    buffer[i++] = u3.as_chars[1];
    buffer[i++] = u3.as_chars[2];
    buffer[i++] = u4.as_int;
    buffer[i++] = u5.as_int;
    buffer[i++] = u6.as_chars[0];
    buffer[i++] = u6.as_chars[1];
    buffer[i++] = u6.as_chars[2];

    u1.as_int = 8;
    buffer[i++] = u1.as_int;

    u1.as_chars[0] = 9;
    buffer[i++] = u1.as_chars[0];

    u4.as_int = 10;
    buffer[i++] = u4.as_int;

    u4.as_chars[1] = 12;
    buffer[i++] = u4.as_chars[1];

    union my_union u7;
    u7 = (union my_union)i;
    buffer[i++] = u7.as_int;
}
