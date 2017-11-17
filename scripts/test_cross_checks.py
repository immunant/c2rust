#!/usr/bin/env python3

import os
import logging
import plumbum as pb

from common import *


def checkout_and_build_libclevrbuf():
    """
    NOTE: we don't need libclevrbuf if we simply pass
    `--features "xcheck-with-dlsym"` in the `runtime/` dir.
    """
    git = get_cmd_or_die("git")
    make = get_cmd_or_die("make")

    if not os.path.isdir(LIBCLEVRBUF_DIR):
        logging.info("fetching ReMon submodule...")
        invoke(git, "submodule", "update", "--init", REMON_SUBMOD_DIR)

    if not os.path.isfile(os.path.join(LIBCLEVRBUF_DIR, "libclevrbuf.so")):
        with pb.local.cwd(LIBCLEVRBUF_DIR):
            invoke(make, "lib")


def test_cross_checks():
    find = get_cmd_or_die("find")
    rustup = get_cmd_or_die("rustup")

    results = find(CROSS_CHECKS_DIR, "-type", "f", "-name", "Cargo.toml")
    lines = results.split(os.linesep)[:-1]  # exclude trailing empty string
    for line in lines:
        rust_proj_path = os.path.dirname(line)
        logging.info("entering %s", rust_proj_path)
        with pb.local.cwd(rust_proj_path):
            args = ["run", CUSTOM_RUST_NAME, "cargo", "test"]
            if rust_proj_path.endswith("runtime"):
                # adding these args avoids taking a dependency on
                # ReMon's libclevrbuf.
                args += ["--features", "xcheck-with-dlsym"]
            invoke(rustup, *args)


def main():
    setup_logging()

    # prerequisites
    if not have_rust_toolchain(CUSTOM_RUST_NAME):
        die("missing rust toolchain: " + CUSTOM_RUST_NAME, errno.ENOENT)

    # checkout_and_build_libclevrbuf()
    test_cross_checks()


if __name__ == "__main__":
    main()
