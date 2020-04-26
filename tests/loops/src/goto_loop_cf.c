void goto_loop(const unsigned int sz, int buffer[const]) {
  
  int i = 0;
  goto l1;

l2:
  buffer[++i] = 2;
  goto l1;

l1:
  buffer[++i] = 1;
  if (i < 10)
    goto l2;

}
