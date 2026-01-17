#include <stdio.h>
#include <assert.h>

static int called = 0;

static int called_only_once(void) {
    called++;
    return 1;
}

int assert_call_only_once(void) {
    assert(called_only_once());
    return called;
}
