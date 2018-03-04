#include <stdbool.h>
#include <string.h>

void sieve_of_eratosthenes(int *buffer) {
    bool prime[102];

    memset(prime, true, sizeof(prime));
    for(int p = 2; p*p <= 101; p++) {
        if (prime[p])
            for (int i = p * 2; i <= 100; i += p)
                prime[i] = false;
    }

    for (int p = 2; p <= 101; p++) {
        if (prime[p])
            buffer[p] = 1;
    }
}
