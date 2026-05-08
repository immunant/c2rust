struct bar {
  int x;
};

int foo() {
  __auto_type x = 42;
  __auto_type px = &x;
  __auto_type sx = sizeof(x);
  __auto_type y = (struct bar) { .x = x };
  return y.x;
}
