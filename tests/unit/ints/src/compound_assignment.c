typedef int myint;
void compound_assignment(const unsigned sz, int buffer[const]) {
       int i = 0;

       unsigned char c = 7;

       c *= 567;
       buffer[i++] = c;
       c /= 567;
       buffer[i++] = c;
       c += 567;
       buffer[i++] = c;
       c -= 567;
       buffer[i++] = c;
       c %= 567;
       buffer[i++] = c;

       buffer[i++] = c *= 567;
       buffer[i++] = c /= 567;
       buffer[i++] = c += 567;
       buffer[i++] = c -= 567;
       buffer[i++] = c %= 567;

       int x = 100;
       x += 2000;
       buffer[i++] = x;

       volatile unsigned char vc = 7;
       vc *= 567;
       buffer[i++] = vc;
       buffer[i++] = vc *= 567;

        int y = 10;
        y *= 500;
        y /= 500;

        myint z = 10;
        z *= 500;
        z /= 500;
}
