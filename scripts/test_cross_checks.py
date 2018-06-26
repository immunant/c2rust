#!/usr/bin/env python3

import errno
import os
import logging
import plumbum as pb

from common import (
    config as c,
    get_cmd_or_die,
    update_or_init_submodule,
    invoke,
    setup_logging,
    have_rust_toolchain,
    die,
)


def checkout_and_build_libclevrbuf():
    """
    NOTE: we don't need libclevrbuf if we simply pass
    `--features "xcheck-with-dlsym"` in the `runtime/` dir.
    """
    make = get_cmd_or_die("make")

    if not os.path.isdir(c.LIBCLEVRBUF_DIR):
        update_or_init_submodule(c.REMON_SUBMOD_DIR)

    if not os.path.isfile(os.path.join(c.LIBCLEVRBUF_DIR, "libclevrbuf.so")):
        with pb.local.cwd(c.LIBCLEVRBUF_DIR):
            invoke(make, "lib")

def build_libfakechecks():
    make = get_cmd_or_die("make")
    if not os.path.isfile(os.path.join(c.LIBFAKECHECKS_DIR, "libfakechecks.so")):
        with pb.local.cwd(c.LIBFAKECHECKS_DIR):
            invoke(make, "all")

def test_clang_cross_checks():
    ninja = get_cmd_or_die("ninja")
    logging.info("entering %s", c.CLANG_XCHECK_PLUGIN_BLD)
    with pb.local.cwd(c.CLANG_XCHECK_PLUGIN_BLD):
        # FIXME: do we really need to clean before every test run???
        invoke(ninja, ["clean"])
        invoke(ninja, ["check-cross-checks"])

def test_rust_cross_checks():
    rustup = get_cmd_or_die("rustup")

    rust_proj_path = os.path.join(c.CROSS_CHECKS_DIR, "rust-checks")
    logging.info("entering %s", rust_proj_path)
    with pb.local.cwd(rust_proj_path):
        invoke(rustup, ["run", c.CUSTOM_RUST_NAME, "cargo", "clean"])
        args = ["run", c.CUSTOM_RUST_NAME, "cargo", "test"]
        invoke(rustup, *args)


def main():
    setup_logging()

    # prerequisites
    if not have_rust_toolchain(c.CUSTOM_RUST_NAME):
        die("missing rust toolchain: " + c.CUSTOM_RUST_NAME, errno.ENOENT)

    # checkout_and_build_libclevrbuf()
    build_libfakechecks()
    test_clang_cross_checks()
    test_rust_cross_checks()


if __name__ == "__main__":
    main()
