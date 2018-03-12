void cast_stuff() {
        int inta[10] = {0};
        int *intp = 0;
        const int *cintp = 0;
        char *x1 = (char*) intp;
        int *x2 = (int*) intp;
        void(*f)(unsigned, int*) = cast_stuff;
        void(*g)(unsigned, int*) = &cast_stuff;
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
        double x12 = (double)10.5f;
}
