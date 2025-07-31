#include <stddef.h>

typedef unsigned long long U64;

#define TEST_FN_MACRO(x) ((x) * (x))

int test_fn_macro(int x) { return TEST_FN_MACRO(x); }

#define TEST_CONST1 1
#define TEST_NESTED 2
#define TEST_CONST2 TEST_NESTED
#define TEST_PARENS (TEST_CONST2 + 1) * 3

int reference_define() {
  int x = TEST_CONST1;
  x += TEST_CONST2;
  if (3 < TEST_PARENS)
    x += TEST_PARENS;
  return x;
}

// Exercise an edge case where a struct initializer needs to be in an unsafe
// block
struct fn_ptrs {
  void *v;
  int (*fn1)(void);
  int (*fn2)(int);
};

typedef int (*fn_ptr_ty)(char);

const struct fn_ptrs fns = {NULL, NULL, NULL};

// Make sure we can't refer to globals in a const macro
#define GLOBAL_REF &fns
const struct fn_ptrs *p = GLOBAL_REF;

// `size_t` not used since it translates differently on macOS,
// and we don't care about testing that here.
// `long` should suffice for testing as well.
// typedef size_t zstd_platform_dependent_type;
typedef long zstd_platform_dependent_type;

#define ZSTD_STATIC_ASSERT(c) (void)sizeof(char[(c) ? 1 : -1])
#define ZSTD_WINDOWLOG_MAX_32 30
#define ZSTD_WINDOWLOG_MAX_64 31
#define ZSTD_WINDOWLOG_MAX                                                     \
  ((int)(sizeof(zstd_platform_dependent_type) == 4 ? ZSTD_WINDOWLOG_MAX_32     \
                                                   : ZSTD_WINDOWLOG_MAX_64))
U64 test_zstd() {
  // This static assert was causing us trouble by somehow giving a valid
  // expression for ZSTD_WINDOWLOG_MAX which shouldn't be possible to translate
  // to a const.
  ZSTD_STATIC_ASSERT(ZSTD_WINDOWLOG_MAX <= 31);
  return ZSTD_WINDOWLOG_MAX;
}

#define inc(ptr)                                                               \
  ({                                                                           \
    (*ptr)++;                                                                  \
    *ptr;                                                                      \
  })

// Ensure the macro generated stmt expr block is codegen'd
int stmt_expr_inc(void) {
  int a = 0;
  int *b = &a;

  // unused
  inc(b);

  // used
  return inc(b);
}

int test_switch(int x) {
  switch (x) {
  case TEST_CONST1:
    return 10;
  case TEST_NESTED:
    return 20;
  }

  return 0;
}

// From #853.
#define silk_int16_MIN ((short)0x8000)

int test_silk_int16_MIN() {
  // _Static_assert((int)silk_int16_MIN == -0x8000, "mistranslated");
  char _null = ""[((int)silk_int16_MIN + 0x8000)];
  return silk_int16_MIN; // Should be -0x8000
}
