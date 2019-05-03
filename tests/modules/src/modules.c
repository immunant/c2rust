//! translate_const_macros, reorganize_definitions

#include <stddef.h>
#include "other_mod2.h"

typedef int char_to_int(char);

int c_to_i(char c) {
  return (int) c;
}

#define FN_PTR_MACRO c_to_i
#define FN_PTR_MACRO2 other_c_to_i

void modules() {
  char x = 10;
  char_to_int* ptr = NULL;

  char_to_int* ptr2 = FN_PTR_MACRO;
  char_to_int* ptr3 = FN_PTR_MACRO2;
  ptr2(x);
}
