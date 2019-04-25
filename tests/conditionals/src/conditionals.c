void entry2(const unsigned buffer_size, int buffer[const]) {

        if (buffer_size < 30) return;
        int i = 0;

        // Test function pointer comparisons
        void(* const f0)(unsigned, int*) = 0;
        void(* const f1)(unsigned, int*) = entry2;
        if (f0) { buffer[i++] = 1; }
        if (f1) { buffer[i++] = 1; }

        if (!f0) { buffer[i++] = 1; }
        if (!f1) { buffer[i++] = 1; }

        if (0.0) { buffer[i++] = 1; }
        if (1.0) { buffer[i++] = 1; }

        if (0) { buffer[i++] = 1; }
        if (1) { buffer[i++] = 1; }

        void *p0 = 0;
        const void *p1 = &buffer_size;

        if (p0) { buffer[i++] = 1; }
        if (p1) { buffer[i++] = 1; }

        // Test simplifiable conditions
        if (0 == 0) { buffer[i++] = 1; }
        if (p1 && 0 > -3) { buffer[i++] = 1; }

        if (!p0) { buffer[i++] = 1; }
        if (!p1) { buffer[i++] = 1; }

        if (p0 && !p0) { buffer[i++] = 1; }
        if (p0 || !p0) { buffer[i++] = 1; }

        if (p1 && !p1) { buffer[i++] = 1; }
        if (p1 || !p1) { buffer[i++] = 1; }
}

// These ternary conditionals require additional parenthesis to compile:
#define CONST 1
#define foo(a) (a ? ((a ? 1 : 2) | 1) : 1)
#define bar(a) (a ? (a ? 1 : 2) : 1)
#define py(flags) \
    ((flags) ? ((((flags)->a & 1) ? 2 : 0) \
              | (((flags)->a & 3) ? 4 : 0) \
              | (((flags)->a & 5) ? 6 : 0) \
               ) : 0)

// Static ternary conditionals (are sectioned)
int abc = (CONST ? 1 : 2) | 2;
int def = CONST ? 1 : 2;
int hij = (CONST ? 1 : 2) + 2;
typedef struct {
    int a;
} py_flag;

// Local ternary conditionals; most need added parens
void ternaries(void) {
    int i = foo(1);
    i = bar(i);
    i = CONST ? 1 : 2; // Doesn't need parens
    i = (CONST ? 1 : 2) | 3;
    i = (CONST ? 1 : 2) + 3;
    py_flag pf = {1};
    i = py(&pf);
}
