void stuff(const char *);

void switch_with_fallthrough() {
    int COND = 0;

    stuff("A");
    switch (COND)
    {
    case 0:
        stuff("B");
    case 1:
        stuff("C");
    case 2:
        stuff("D");
    default:
        stuff("E");
    }
    stuff("F");
}
