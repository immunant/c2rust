# Translating json-c

    # in examples/json-c/repo:
    ../configure    # use the custom c2rust configure script
    bear make
    make check
    python3 ../translate.py
    ninja -C rust

This will produce `rust/libjson-c.so.4.0.0`.


# Running tests

    # in examples/json-c/repo:

    # Replace the C libjson-c.so with a symlink to the Rust one.
    # You only need to do this the first time.
    rm .libs/libjson-c.so.4.0.0
    ln -s ../rust/libjson-c.so.4.0.0 .libs/libjson-c.so.4.0.0

    # Run tests
    make check

If you modify the C files, `make check` will try to rebuild some stuff and then
will break because of the object files that `translate.py` deleted.  If this
happens, run `make clean && make`, then repeat the "running tests" steps from
the top.

