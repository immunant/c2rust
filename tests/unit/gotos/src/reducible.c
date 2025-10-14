void stuff(const char *) {}

// A simplified version of `irreducible.c` that removes one of the `goto`s.
void probably_reducible() {
    int COND = 0;

A:
    // A
    stuff("A");
    if (COND) {
        // B
        stuff("B");
        // The original does goto G;
    }
    // C
    stuff("C");

D:
    // D
    stuff("D");
    if (COND) {
        // E
        stuff("E");
        goto A;
    }
    // F
    stuff("F");

G:
    // G
    stuff("G");
    if (COND) {
      // H
      stuff("H");
      goto D;
    }
    // I
    stuff("I");
}
