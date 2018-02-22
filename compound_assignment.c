void entry(const unsigned sz, int buffer[const]) {
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
}
