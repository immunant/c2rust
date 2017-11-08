/* Define `entry` function solely to avoid linker errors */
void entry(unsigned buffer_size, int buffer[]) {

}

/*
 * Real function to test. Clang handles functions w/o
 * arguments specially, which is why we need this test.
 */
int nofnargs() {
    return 0;
}