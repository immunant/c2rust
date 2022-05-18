/* asm.c */
#include <stdint.h>

// from https://www.ibiblio.org/gferg/ldp/GCC-Inline-Assembly-HOWTO.html#s2

/* We want to multiply a number by 5. For that we use the instruction lea. */
int mul5_1(int x) {
    int five_times_x;
    asm ("leal (%k1,%k1,4), %k0"
         : "=r" (five_times_x)
         : "r" (x)
         );
    return five_times_x;
}

/* Here our input is in ’x’. We didn’t specify the register to be used. GCC will choose some register for input, one for output and does what we desired. If we want the input and output to reside in the same register, we can instruct GCC to do so. Here we use those types of read-write operands. By specifying proper constraints, here we do it. */

int mul5_2(int x) {
    int five_times_x;
            asm ("leal (%k0,%k0,4), %k0"
                 : "=r" (five_times_x)
                 : "0" (x) 
                 );
    return five_times_x;
}

// Now the input and output operands are in the same register. But we don’t know which register. Now if we want to specify that also, there is a way.

int mul5_3(int x) {
            asm ("leal (%%ecx,%%ecx,4), %%ecx"
                 : "=c" (x)
                 : "c" (x)
                 );
    return x;
}

int mul2(int var64)
{
    int out;
    asm("add %0, %1\n\t"
        : "=r"(out)
        : "0"(var64));
    return out;
}


int mul2_2(int var64)
{
    int out;
    int dummy = 2;
    asm("add %0, %2\n\t"
        "add %1, %1"
        : "=r"(out)
        : "r"(dummy), "0"(var64));
    return out;
}

int six(void) {
    int out = 0;
    int zero = 0;
    int six = 6;

    // tests AT&T syntax default without extra context
    asm("add %1, %0\n\t"
        : "=r"(out)
        : "r"(six), "0"(zero));

    return out;
}

void entry(const unsigned int buffer_size, int buffer[const])
{
    int i = 0;

    buffer[i++] = mul5_1(48605);
    buffer[i++] = mul5_2(13014);
    buffer[i++] = mul5_3(10290);
    buffer[i++] = mul2(6);
    buffer[i++] = mul2_2(6);
    buffer[i++] = six();
}
