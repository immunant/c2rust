//! translate_const_macros, reorganize_definitions, emit_build_files
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
#define CONSTANT 0 /* comment at the end of define */

/* something in between */

/* comment on a function
 * with a second line */
int test_fn() {
  /* comment in a function */

  /* another */
  int x = CONSTANT; /* at end of statement line */

  /* comment on empty stmt */
  (void)x; /* trailing comment on empty stmt */

  /* before while */
  while (x < 5) { /* trailing block begin */
    /* in while */
    x++;
    /* end of while */
  }

  if (x > 100) {
    /* in an if */
    x = 10;
    /* end of if */
  } else if (x > 200) {
    /* in else if */
    x = 12;
    /* end of else if */
  } else {
    /* in else */
    x = x;
    /* end of else */
  }

  /* quake3 style */
  if (x > 50)
  {
    x = 20;
  } //end if
  else if (x > 75)
  {
    x = 13;
  } //end else if

  /* before a statement */
  return x + CONSTANT1;

  /* after all statements */
  /** even a double comment is not turned into a doc comment *//** */
}

/* after all functions */
