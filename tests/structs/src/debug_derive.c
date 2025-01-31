//! derive:Debug

#include <stdarg.h>

typedef struct {
    int a;
} Debuggable1;

typedef struct {
    va_list v;
} Debuggable2;

Debuggable1 *kDebuggable1;
Debuggable2 *kDebuggable2;

// A struct containing a union cannot derive Debug, so make
// sure we don't generate a #[derive] for it
typedef struct {
    struct {
        struct {
            union {
                int d;
                float e;
            } c;
        } b;
    } a;
} NotDebuggable1;

NotDebuggable1 kNotDebuggable1;
