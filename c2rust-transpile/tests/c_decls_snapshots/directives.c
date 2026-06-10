// comment for cond_fn
int cond_fn(void) {
#ifdef NOT_DEFINED
    /* comment in inactive region; must not appear in preprocessed text */
    return 1;
#else
    // comment in active region
    return 2;
#endif
}

#if 0
/* entirely disabled function */
int disabled_fn(void) { return 3; }
#endif

#define DOUBLE(x) ((x) + (x))

/* comment for uses_macro */
int uses_macro(int n) {
    /* macro invocation should stay unexpanded */
    return DOUBLE(n);
}
