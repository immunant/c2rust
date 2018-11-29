#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys
import plumbum as pb

from common import (
    config as c,
    get_cmd_or_die,
    die,
    invoke,
    on_mac,
)


def main():
    cargo = get_cmd_or_die("cargo")
    args = ["build", "--features", "llvm-static", ]

    llvm_config = os.path.join(c.LLVM_BLD, "bin/llvm-config")
    if not os.path.isfile(llvm_config):
        emsg = "can't find `llvm-config`; try running build_translator.py first.\n"
        emsg += "expected location: " + llvm_config
        die(emsg)

    if on_mac():
        llvm_system_libs = "-lz -lcurses -lm -lxml2"
    else:  # linux
        llvm_system_libs = "-lz -lrt -ltinfo -ldl -lpthread -lm"

    llvm_libdir = os.path.join(c.LLVM_BLD, "lib")
    assert os.path.isdir(llvm_libdir), \
        "LLVM libdir not found ({})".format(llvm_libdir)
    with pb.local.cwd(c.C2RUST_DIR):
        with pb.local.env(RUST_BACKTRACE="1",
                          LLVM_CONFIG_PATH=llvm_config,
                          LLVM_SYSTEM_LIBS=llvm_system_libs,
                          C2RUST_AST_EXPORTER_LIB_DIR=llvm_libdir):
            # build
            retcode = cargo[args] & pb.RETCODE(FG=True)
            if retcode != 0:
                exit(retcode)
            # run
            # args = ["run", "--bin", "c2rust"] + args[1:] + ["--"]
            # args += sys.argv[1:]
            # cargo[args] & pb.FG(retcode=None)

            c2rust = pb.local.get("../target/debug/c2rust")
            c2rust[sys.argv[1:]] & pb.FG(retcode=None)


if __name__ == "__main__":
    main()
