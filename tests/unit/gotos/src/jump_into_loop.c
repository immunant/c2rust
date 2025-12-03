#include <stdlib.h>

// This used to cause the translator to crash at translation time.
// It doesn't do anything, but it does have interesting control-flow.
int jump_into_loop() {
    int x = 0;

    while (x < 10) {
        if (x > 11) {
            foo:
                exit(x);
        }
    }
    
    x = 1; 

    goto foo;

    return 1;
}
