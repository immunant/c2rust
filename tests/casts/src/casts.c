#include <stdlib.h>
#include <stdbool.h>

void cast_stuff(void) {
        int inta[10] = {0};
        int *intp = 0;
        const int *cintp = 0;
        char *x1 = (char*) intp;
        int *x2 = (int*) intp;
        void(*f)(unsigned, int*) = (void(*)(unsigned, int*))cast_stuff;
        void(*g)(unsigned, int*) = (void(*)(unsigned, int*))&cast_stuff;
        //(union intfloat)1;
        int *x3 = (int*)inta;
        int *x4 = (int*)0;
        int *x5 = (int*)1;
        int x6 = (int)intp;
        (void)intp;
        long x7 = (long)10;
        float x8 = (float)10;
        int x9 = (int)10.0;
        _Bool x10 = (_Bool)10;
        _Bool x11 = (_Bool)10.5;
        _Bool x12 = NULL;
        double x13 = (double)10.5f;

        // const casts in LLVM 8 are now NoOp casts instead of BitCasts, so we
        // need to make sure we handle this correctly.
        const int const_i = -1;
        int *x14 = (int*) &const_i;

        bool b = true;
        float x15 = b;
        void* x16 = (void*)b;

        // reference applied to a cast requires parens on Rust side
        int * x = &(int){0};
}
