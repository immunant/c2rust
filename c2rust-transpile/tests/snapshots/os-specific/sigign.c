#include <signal.h>

void foo(void)
{
    void (*handler)(int) = SIG_IGN;
}
