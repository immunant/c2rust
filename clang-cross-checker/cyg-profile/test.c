#include <stdio.h>
#include <stdint.h>

uint64_t fibo(uint64_t n) {
    if (n <= 1) {
        return 1;
    } else {
        return fibo(n - 1) + fibo(n - 2);
    }
}

int main() {
    printf("Profiling test!!!\n");
    fibo(4);
    return 0;
}
