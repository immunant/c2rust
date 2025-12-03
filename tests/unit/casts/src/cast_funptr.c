#include <stdlib.h>

int identity(int i) { return i; }

// dlsym also returns void*, it's not generally safe
// to cast a void pointer to a function pointer but
// I believe that POSIX mandates that you can do it.
void *get_identity(void) { return identity; }

void entry(const unsigned sz, int buffer[const]) {
        typeof(identity) * f = get_identity();
        buffer[0] = f(10);
        buffer[1] = (size_t)f;
}
