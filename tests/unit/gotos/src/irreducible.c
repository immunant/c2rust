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

// TODO: We want a snapshot test of this case to verify that we don't generate
// excessive blocks when jumping to a loop with multiple entries.
int jump_to_irreducible(int x) {
  switch (x) {
    case 0:
      x++;
      goto l1;

    case 1:
      x++;
      goto l2;

    case 2:
      x++;
      goto l3;
    }

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
