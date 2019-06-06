//! translate_const_macros, reorganize_definitions
/* top level doc comment
 * second line
 */

#include "comments.h"

int a_function() {
  return 1;
}

/* another comment */
/* and another */

/* one more */
#define CONSTANT 0

/* something in between */

/* comment on a function
 * with a second line */
int test_fn() {
  /* comment in a function */

  /* another */

  /* before a statement */
  return CONSTANT + CONSTANT1;

  /* after all statements */
}

/* after all functions */
