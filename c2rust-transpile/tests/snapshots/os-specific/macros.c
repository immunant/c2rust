#include <errno.h>
#include <stdbool.h>

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
