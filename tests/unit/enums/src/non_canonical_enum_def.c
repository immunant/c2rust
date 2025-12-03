// In this case the extern fn creates the canonical enum decl
// and the enum definition is non canonical

extern enum hrtimer_restart it_real_fn(void);

enum hrtimer_restart {
    HRTIMER_NORESTART,
    HRTIMER_RESTART,
};

void abc(void) {
    it_real_fn();
}
