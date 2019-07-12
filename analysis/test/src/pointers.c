#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>

struct S *global;

struct S {
  int field;
};

void exercise_allocator() {
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
}

void simple_analysis() {
  struct S* s = (struct S*)malloc(sizeof(struct S));
  s->field = 10;
  printf("%i\n", s->field);
  free(s);
}

void analysis2_helper(struct S *s) {
  printf("%i\n", s->field);
}

void analysis2() {
  struct S* s = (struct S*)malloc(sizeof(struct S));
  s->field = 10;
  analysis2_helper(s);
  free(s);
}

void no_owner(int should_free) {
  global = (struct S*)malloc(sizeof(struct S));
  if (should_free)
    free(global);
}

void invalid() {
  struct S* s = (struct S*)malloc(sizeof(struct S));
  s->field = 10;
  global = s;
  printf("%i\n", s->field);
  printf("%i\n", global->field);
  global = NULL;
  free(s);
}

int main(int argc, char *argv[]) {
  exercise_allocator();
  simple_analysis();
  analysis2();
  no_owner(0);
  no_owner(1);
  invalid();
  return 0;
}
