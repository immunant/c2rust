/* Thread-local so the parallel test harness gives each test its own trace
 * instead of racing on shared state (both here and in the transpiled Rust). */
static _Thread_local int trace_buf[16];
static _Thread_local int trace_n;

static void reset(void) {
    trace_n = 0;
}

static void copy_trace(int *out, int *n) {
    int i;
    for (i = 0; i < trace_n; i++) out[i] = trace_buf[i];
    *n = trace_n;
}

static void record(int *p) {
    if (trace_n < 16) trace_buf[trace_n++] = *p;
}

/* Each `do_*` helper runs the cleanup-bearing code in its own function so
 * that all function-scope cleanups complete on return; the outer `run_*`
 * then copies the recorded trace. */

static void do_single(void) {
    int x __attribute__((cleanup(record))) = 5;
    (void)x;
}

void run_single(int *out, int *n) {
    reset();
    do_single();
    copy_trace(out, n);
}

static void do_multiple(void) {
    int a __attribute__((cleanup(record))) = 1;
    int b __attribute__((cleanup(record))) = 2;
    int c __attribute__((cleanup(record))) = 3;
    (void)a; (void)b; (void)c;
}

void run_multiple(int *out, int *n) {
    reset();
    do_multiple();
    copy_trace(out, n);
}

static void do_early_return(void) {
    int x __attribute__((cleanup(record))) = 7;
    (void)x;
    return;
}

void run_early_return(int *out, int *n) {
    reset();
    do_early_return();
    copy_trace(out, n);
}

static void do_nested(void) {
    int outer __attribute__((cleanup(record))) = 10;
    {
        int inner __attribute__((cleanup(record))) = 20;
        (void)inner;
    }
    int after = 30;
    record(&after);
    (void)outer;
}

void run_nested(int *out, int *n) {
    reset();
    do_nested();
    copy_trace(out, n);
}

/* Back-edge via goto forces c2rust to hoist locals. The cleanup variable
 * is naturally reached, but its declaration sits in a hoisted block, so
 * this verifies the guard runs on the assignment site rather than on the
 * bare zero-init at function top. */
static void do_goto(void) {
    int counter __attribute__((cleanup(record))) = 0;
again:
    counter++;
    if (counter < 3) goto again;
}

void run_goto(int *out, int *n) {
    reset();
    do_goto();
    copy_trace(out, n);
}

typedef struct {
    int v;
} widget_t;

static void widget_destroy(widget_t *w) {
    record(&w->v);
}

static void do_typedef(void) {
    widget_t w __attribute__((cleanup(widget_destroy))) = { 42 };
    (void)w;
}

void run_typedef(int *out, int *n) {
    reset();
    do_typedef();
    copy_trace(out, n);
}
