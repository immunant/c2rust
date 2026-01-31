#!/usr/bin/env python3

import os
import plumbum as pb
import sys

from common import (
    config as c,
    get_cmd_or_die,
    invoke,
)
from test_cross_checks import build_libfakechecks

def wrap_cc():
    cc_wrapper_args = sys.argv[1:]

    plugin_so = os.path.join(c.CLANG_XCHECK_PLUGIN_BLD,
                             "plugin", "CrossChecks.so")
    runtime_a = os.path.join(c.CLANG_XCHECK_PLUGIN_BLD,
                             "runtime", "libruntime.a")
    libfakechecks_so = os.path.join(c.LIBFAKECHECKS_DIR, "libfakechecks.so")

    cc_args = [
        "-I", os.path.join(c.CLANG_XCHECK_PLUGIN_SRC, "include"),
        "-DC2RUST_CROSS_CHECK_VALUE_REAL",
        "-Xclang=-load", f"-Xclang={plugin_so}",
        "-Xclang=-add-plugin", "-Xclang=crosschecks",
        "-Xclang=-plugin-arg-crosschecks", "-Xclang=--disable-xchecks",
    ]
    # Add the wrapper arguments
    cc_args.extend(sys.argv[1:])
    # Add the libraries
    cc_args += [runtime_a, libfakechecks_so]

    clang = get_cmd_or_die("clang")
    invoke(clang[cc_args])

def main():
    build_libfakechecks()
    wrap_cc()


if __name__ == "__main__":
    main()
