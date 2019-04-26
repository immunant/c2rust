//! translate_const_macros

#define TEST_CONST1 1
#define TEST_NESTED 2
#define TEST_CONST2 TEST_NESTED
#define TEST_PARENS (TEST_CONST2 + 1) * 3
#define NULL 0

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
  int (*fn1)(void);
  int (*fn2)(int);
};

struct fn_ptrs const fns = {NULL, NULL};


// Make sure we can't refer to globals in a const macro
#define GLOBAL_REF &fns

struct fn_ptrs* p = GLOBAL_REF;
