int printf(const char*, ...);

struct PacketHeader {
    unsigned char version : 3;
    unsigned char type : 2;
    unsigned char flags : 3;
    unsigned short sequence : 10;
    unsigned short length : 6;
};

struct SingleRawIdent {
    signed char as : 3;
};

struct PacketHeader* get_h(void);

void test_bitfields(void) {
    struct PacketHeader h = {0};
    h.version = 5;
    h.sequence = 513;
    printf("version=%u sequence=%u\n", h.version, h.sequence);
    struct SingleRawIdent s = {0};

    signed char prefix_signed = ++s.as;
    signed char postfix_signed = s.as++;
    unsigned char prefix_unsigned = ++get_h()->version;
    unsigned char postfix_unsigned = get_h()->version++;
}
