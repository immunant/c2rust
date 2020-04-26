//! allow_current_block
void goto_switch(unsigned buffer_size, int buffer[]) {

  int i = -3;

l1:
  switch (i) {
    default:
      ++i;
      buffer[i+3] = 1;
      goto l1;
    case 0:
      ++i;
      buffer[i+3] = 2;
    case 4:
    case 1:
      ++i;
      buffer[i+3] = 3;
      break;
    case 3:
      buffer[i+3] = 4;
      break;
  }

}


