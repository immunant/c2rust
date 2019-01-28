/*
 * Real function to test. Clang handles functions w/o
 * arguments specially, which is why we need this test.
 *
 * The correct definition is "int nofnargs(void)", but
 * it's common for people to miss this and fallback to
 * previous C behavior.
 */
int nofnargs() {
    return 0;
}
