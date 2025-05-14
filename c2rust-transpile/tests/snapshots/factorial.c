unsigned short factorial(unsigned short n) {
    unsigned short result = 1;
    for (unsigned short i = 1; i < n; i++) {
        result *= i;
    }
    return result;
}
