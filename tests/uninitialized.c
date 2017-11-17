typedef int myint;
typedef myint *myintp;

void entry(const unsigned sz, int buf[const]) {
       myint x;
       myintp p;
       p = &x;
       *p = 1;
       if (sz > 0) { *buf = x; }

       const int * const q1;
       const int * q2;
       int * const q3;
       int * q4;
       void (*f)(void);
       void (*fs[2])(void);
       int xs[10];
       int ys[4][5];
}
