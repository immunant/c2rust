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

// Tests that when we have jumps to an irreducible loop we don't generate
// excessive labeled blocks.
int jump_to_irreducible(int x) {
    switch (x) {
    case 0:
        if (x)
            goto a;
        else
            break;
    case 1:
        if (x)
            goto b;
        else
            break;
    default:
        x += 1;
    }

    x += 2;

a:
    if (x) {
        goto b;
    } else {
        return x;
    }

b:
    if (x) {
        goto a;
    } else {
        return x;
    }
}
