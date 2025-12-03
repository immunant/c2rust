/* This should translate cleanly with a single extern function definition. */
void coreutils_static_assert() {
  enum { count = 2 };

  /* Expanded form of verify (count > 1); from coreutils/lib/verify.h
     This is a declaration of _gl_verify_function2 as an extern function (void)
     returning a pointer to array of size 1 of int */
  extern int (*_gl_verify_function2 (void)) [(!!sizeof (struct { unsigned int _gl_verify_error_if_negative: (count > 1) ? 1 : -1; }))];
}

/* Clang doesn't support nested functions */
/*
void nested() {
  void inner_function() {
    printf("in inner");
  }

  inner_function();
}
*/
