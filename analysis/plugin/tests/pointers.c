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
  printf("%i\n", s[0].field);
  printf("%i\n", s[1].field);
  return 0;
}
