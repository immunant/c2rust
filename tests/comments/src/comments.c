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
  int x = CONSTANT;
  /* before while */
  while (x < 5) {
    /* in while */
    x++;
    /* end of while */
  }

  if (x > 100) {
    /* in an if */
    x = 10;
    /* end of if */
  }

  /* before a statement */
  return x + CONSTANT1;

  /* after all statements */
}

/* after all functions */
