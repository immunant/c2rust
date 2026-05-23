int printf(const char*, ...);

struct PacketHeader {
    unsigned char version : 3;
    unsigned char type : 2;
    unsigned char flags : 3;
    unsigned short sequence : 10;
    unsigned short length : 6;
};

struct SingleRawIdent {
    unsigned char as : 3;
};

void test_bitfields(void) {
    struct PacketHeader h = {0};
    h.version = 5;
    h.sequence = 513;
    printf("version=%u sequence=%u\n", h.version, h.sequence);
    struct SingleRawIdent s = {0};
}
