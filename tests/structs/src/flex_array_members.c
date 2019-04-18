#include <stdlib.h>

// Clang does not allow a flexible array member as the sole field in a struct

struct gnu_flex {
  int x;
  int flex[0];
};

struct c99_flex {
  int x;
  int flex[];
};

struct c89_flex {
  int x;
  int flex[1];
};

// TODO(C++): Need to handle flex arrays in unions per g++ behavior

void exercise_flex_arrays(const unsigned sz, int buf[const]) {
  int i = 0;

  struct gnu_flex *s = (struct gnu_flex*) calloc(sizeof(int), 11);
  buf[i++] = s->flex[0];
  buf[i++] = s->flex[1];
  buf[i++] = s->flex[2];

  s->flex[5] = 10;
  buf[i++] = s->flex[5];

  struct c99_flex *t = (struct c99_flex*) calloc(sizeof(int), 11);
  buf[i++] = t->flex[0];
  buf[i++] = t->flex[1];
  buf[i++] = t->flex[2];

  t->flex[5] = 10;
  buf[i++] = t->flex[5];

  struct c89_flex *u = (struct c89_flex*) calloc(sizeof(int), 11);
  buf[i++] = u->flex[0];
  buf[i++] = u->flex[1];
  buf[i++] = u->flex[2];

  u->flex[0] = 10;
  u->flex[5] = 15;
  buf[i++] = u->flex[5];
}
