#include "other_mod.h"

int (*global_fn)(char) = other_c_to_i;

E_enum use_enum() {
  int x = E_variant1;
  return (E_enum) 2;
}
