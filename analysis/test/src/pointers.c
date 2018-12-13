#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>

struct S {
  int field;
};

int main(int argc, char *argv[]) {
  struct S* s = (struct S*)malloc(sizeof(struct S));
  s->field = 10;
  printf("%i\n", s->field);

  s = (struct S*)realloc(s, 2*sizeof(struct S));
  s[0].field = 10;
  s[1].field = 11;
  for (int i = 0; i < 2; ++i)
    printf("%i\n", s[i].field);

  s = (struct S*)reallocarray(s, 3, sizeof(struct S));
  s[0].field = 10;
  s[1].field = 11;
  s[2].field = 12;
  for (int i = 0; i < 3; ++i)
    printf("%i\n", s[i].field);

  free(s);

  s = (struct S*)calloc(4, sizeof(struct S));
  s[0].field = 10;
  s[1].field = 11;
  s[2].field = 12;
  s[3].field = 13;
  for (int i = 0; i < 4; ++i)
    printf("%i\n", s[i].field);

  free(s);

  return 0;
}
