/* test.c */
typedef unsigned int uint32_t;
typedef unsigned char uint8_t;

uint32_t __STREXB(uint8_t value, volatile uint8_t *addr)
{
   uint32_t result;

   asm volatile ("strexb %0, %2, %1" : "=&r" (result), "=Q" (*addr) : "r" ((uint32_t)value) );
   return(result);
}

void entry(const unsigned int buffer_size, int buffer[const])
{
    int i = 0;

    //uint32_t 0xfedcba98
    uint8_t dest = 0;
    __STREXB(5, &dest);
    buffer[i++] = dest;
}
