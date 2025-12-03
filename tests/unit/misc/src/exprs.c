int side_effect(int* x) {
        *x = 1;
        return 0;
}

void exprs(const unsigned n, int * const buffer) {

        unsigned long i = 0;
        char arr[1] = {0};

        side_effect(&buffer[0]);
        -side_effect(&buffer[1]);
        &""[side_effect(&buffer[2])];
        ++arr[side_effect(&buffer[3])];
}
