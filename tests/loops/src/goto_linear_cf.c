void goto_linear(const unsigned int sz, int buffer[const]) {
  int i = 0;
  buffer[++i] = 1;
  goto l1;

l2:
  buffer[++i] = 2;
  return;

l1:
  buffer[++i] = 3;
  goto l2;
}
