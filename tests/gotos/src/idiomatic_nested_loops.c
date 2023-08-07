// This should translate to a straightforward `break` with a loop label 
int break_multiple(int x) {
  /* comment1 */

    while (x < 5) {
        /* comment2 */
        while (x < 5) {
            /* comment3 */
            while (x < 5) {
                /* comment4 */
                if (x < 2)
                    /* comment5 */
                    goto break_outer;
                /* comment6 */
                x++;
                /* comment7 */
            }
            /* comment8 */
        }
        /* comment9 */
    }
    /* comment10 */

break_outer:
    /* comment11 */
    x += 4;

    /* comment12 */
    return x;
    /* comment13 */
}
