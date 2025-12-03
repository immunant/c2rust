//! translate_const_macros

#define TEST_FN_MACRO(x) ((x) * (x))

int test_fn_macro(int x) {
  return TEST_FN_MACRO(x);
}

#include <stddef.h>
#include <stdint.h>
typedef  uint64_t U64;
typedef  uint32_t U32;

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

struct fn_ptrs const fns = {NULL, NULL, NULL};


// Make sure we can't refer to globals in a const macro
#define GLOBAL_REF &fns
struct fn_ptrs* p = GLOBAL_REF;

#define ZSTD_STATIC_ASSERT(c) (void)sizeof(char[(c) ? 1 : -1])
#define ZSTD_WINDOWLOG_MAX_32    30
#define ZSTD_WINDOWLOG_MAX_64    31
#define ZSTD_WINDOWLOG_MAX     ((int)(sizeof(size_t) == 4 ? ZSTD_WINDOWLOG_MAX_32 : ZSTD_WINDOWLOG_MAX_64))
U64 test_zstd() {
  // This static assert was causing us trouble by somehow giving a valid
  // expression for ZSTD_WINDOWLOG_MAX which shouldn't be possible to translate
  // to a const.
  ZSTD_STATIC_ASSERT(ZSTD_WINDOWLOG_MAX <= 31);
  return ZSTD_WINDOWLOG_MAX;
}

#define inc(ptr) ({\
  (*ptr)++;\
  *ptr;\
})

// Ensure the macro generated stmt expr block is codegen'd
int stmt_expr_inc(void) {
  int a = 0;
  int* b = &a;

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

#ifndef __APPLE__
// `long double` translated as `f128` cannot be used in const macros because
// `f128::f128::new` is not `const`; ensure we do not constify macros that
// create long doubles via casts or literals.
long double returns_longdouble() { return 0.0L; }
int returns_int() { return 1; }

#define LONGDOUBLE_LIT 0.5L
#define GENERATES_CAST (returns_longdouble() + returns_int())

void long_doubles(void) {
    long double ld = LONGDOUBLE_LIT;
    long double ld2 = GENERATES_CAST;
}
#endif
