#include <errno.h>
#include <stdbool.h>

bool errno_is_error() { return errno != 0; }
