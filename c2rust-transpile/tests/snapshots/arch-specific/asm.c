#ifdef __aarch64__

// nothing here yet...

#endif

#ifdef __x86_64__

// Tests https://github.com/immunant/c2rust/issues/398
int six(void) {
    int out = 0;
    int six = 6;

    asm (
        "add %0, %1\n\t"
        : "=r"(out)
        : "r"(six), "0"(out)
    );

    return out;
}

#endif
