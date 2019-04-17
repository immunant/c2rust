#include <stdlib.h>

struct c99_flex {
  int x;
  int flex[];
};

struct old_style_flex {
  int x;
  int flex[1];
};

void exercise_flex_arrays(const unsigned sz, int buf[const]) {
  int i = 0;

  struct c99_flex *s = (struct c99_flex*) calloc(sizeof(int), 10);
  buf[i++] = s->flex[0];
  buf[i++] = s->flex[1];
  buf[i++] = s->flex[2];

  s->flex[5] = 10;
  buf[i++] = s->flex[5];

  struct old_style_flex *t = (struct old_style_flex*) calloc(sizeof(int), 10);
  buf[i++] = t->flex[0];
  buf[i++] = t->flex[1];
  buf[i++] = t->flex[2];

  t->flex[0] = 10;
  t->flex[5] = 15;
  buf[i++] = t->flex[5];
}
