//! translate_const_macros

#define TEST_CONST1 1
#define TEST_NESTED 2
#define TEST_CONST2 TEST_NESTED + 1

int reference_define() {
  int x = TEST_CONST1;
  x += TEST_CONST2;
  return x;
}
