#include <errno.h>
#include <stdbool.h>
#include <string.h>

bool errno_is_error() { return errno != 0; }

int size_of_const() {
  int a[10];
#define SIZE sizeof(a)
  return SIZE;
}

#if 0
// TODO VLA error
int size_of_dynamic(int n) {
  int a[n];
#define SIZE sizeof(a)
  return SIZE;
}
#endif

// From Lua's `lobject.c`.

#define POS "\"]"
/* number of chars of a literal string without the ending \0 */
#define LL(x) (sizeof(x) / sizeof(char) - 1)

void memcpy_str_literal(char *out) {
  memcpy(out, POS, (LL(POS) + 1) * sizeof(char));
}
