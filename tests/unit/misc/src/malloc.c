#include <stdlib.h>

void malloc_test(const unsigned int buffer_size, int buffer[const])
{
        int i = 0;

        void **p = malloc(sizeof(*p));
        free(p);

        int *ip = malloc(sizeof(*ip));
        *ip = 34;
        buffer[i++] = *ip;

        ip = realloc(ip, 2 * sizeof(*ip));
        ip[0] = 35;
        ip[1] = 36;
        buffer[i++] = ip[0];
        buffer[i++] = ip[1];

        free(ip);
}
