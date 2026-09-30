// Const-like macros that dereference a pointer, e.g. memory-mapped registers
// as defined by MCU vendor headers, must be inlined at each use site, not
// emitted as a `const` (dereferencing a raw pointer isn't `const`).

struct regs {
    unsigned int data;
};

#define REG (*(volatile unsigned int *)0x40000000)
#define REG_ARRAY ((volatile unsigned int *)0x40000000)
#define REG_INDEXED (REG_ARRAY[1])
#define REG_STRUCT ((volatile struct regs *)0x40000000)
#define REG_MEMBER (REG_STRUCT->data)

void set_bits(void) {
    REG |= 1;
    REG_INDEXED |= 2;
    REG_MEMBER |= 4;
}

// Increments and decrements modify their operand, so they aren't `const`.

static int counter;

#define NEXT_COUNTER (counter++)

int next_counter(void) {
    return NEXT_COUNTER;
}
