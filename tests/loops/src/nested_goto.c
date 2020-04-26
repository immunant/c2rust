#include <stdio.h>

// Adapted from gnulib add_exclude_fp
// Regression test for incremental relooper
void test_nested_with_goto (int n, int x, char* buf) {
  int i;
  for (i = 0; i < n; i++)
    if (n == 10) {
      if (x < 100) {
        for (;; x--)
          if (x == i)
            goto next;
          else if (x == 0)
            break;
      }

      printf("didn't hit goto");
      next:
      printf("after label");
    }
}
