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

void entry(const unsigned int sz, int buf[const])
{
        if (sz < 30) return;
        int i = 0;

        union my_union u1 = { .as_int = 1 };
        union my_union u2 = { 2 };
        union my_union u3 = { .as_chars = {3,4} };

        union my_union_flipped u4 = { .as_int = 5 };
        union my_union_flipped u5 = { 6 };
        union my_union_flipped u6 = { .as_chars = {7,8} };


        buf[i++] = sizeof(union my_union);
        buf[i++] = sizeof(union my_union_flipped);
        buf[i++] = sizeof(union empty_union);

        buf[i++] = u1.as_int;

        buf[i++] = u2.as_int;

        buf[i++] = u3.as_chars[0];
        buf[i++] = u3.as_chars[1];
        buf[i++] = u3.as_chars[2];

        buf[i++] = u4.as_int;

        buf[i++] = u5.as_int;

        buf[i++] = u6.as_chars[0];
        buf[i++] = u6.as_chars[1];
        buf[i++] = u6.as_chars[2];

        u1.as_int = 8;
        buf[i++] = u1.as_int;

        u1.as_chars[0] = 9;
        buf[i++] = u1.as_chars[0];

        u4.as_int = 10;
        buf[i++] = u4.as_int;

        u4.as_chars[1] = 12;
        buf[i++] = u4.as_chars[1];

        union my_union u7;
        u7 = (union my_union)i;
        buf[i++] = u7.as_int;
}
