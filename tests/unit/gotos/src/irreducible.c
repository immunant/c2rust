//! allow_current_block
int irreducible(int x) {

l1:
    if (x < 6) {
      x += 1;
      goto l3;
    }

l2:
    if (x < 9) {
      x += 2;
      goto l1;
    }

l3:
    if (x < 20) {
      x += 90;
      goto l2;
    }

    return x;
}
