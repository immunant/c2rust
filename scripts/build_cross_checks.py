#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import re
import sys
import errno
import shutil
import logging
import argparse


from common import (
    config as c,
    pb,
    die,
    get_cmd_or_die,
    est_parallel_link_jobs,
    invoke,
    setup_logging,
    have_rust_toolchain,
    ensure_clang_version,
    ensure_dir,
    git_ignore_dir,
    get_ninja_build_type,
    on_mac,
)


def build_clang_plugin(args: str) -> None:
    """
    run cmake as needed to generate ninja buildfiles. then run ninja.
    """
    cargo = get_cmd_or_die("cargo")
    config_capi_src_dir = os.path.join(c.CROSS_CHECKS_DIR, "rust-checks", "config-capi")
    cargo_target_dir = os.path.join(c.CLANG_XCHECK_PLUGIN_BLD,
            "config-capi-target")
    config_lib_path = os.path.join(cargo_target_dir,
            "debug" if args.debug else "release",
            "libc2rust_xcheck_config_capi.a")
    with pb.local.cwd(config_capi_src_dir):
        cargo_args = ["build", "--package", "c2rust-xcheck-config-capi"]
        if not args.debug:
            cargo_args.append("--release")
        with pb.local.env(CARGO_TARGET_DIR=cargo_target_dir):
           invoke(cargo[cargo_args])

    ninja = get_cmd_or_die("ninja")
    # Possible values are Release, Debug, RelWithDebInfo and MinSizeRel
    build_type = "Debug" if args.debug else "RelWithDebInfo"
    ninja_build_file = os.path.join(c.CLANG_XCHECK_PLUGIN_BLD, "build.ninja")
    with pb.local.cwd(c.CLANG_XCHECK_PLUGIN_BLD):
        if os.path.isfile(ninja_build_file):
            prev_build_type = get_ninja_build_type(ninja_build_file)
            run_cmake = prev_build_type != build_type
        else:
            run_cmake = True

        if run_cmake:
            cmake = get_cmd_or_die("cmake")
            max_link_jobs = est_parallel_link_jobs()
            cargs = ["-G", "Ninja", c.CLANG_XCHECK_PLUGIN_SRC,
                     "-DXCHECK_CONFIG_LIB={}".format(config_lib_path),
                     "-DCMAKE_BUILD_TYPE=" + build_type,
                     "-DBUILD_SHARED_LIBS=1",
                     "-DLLVM_PARALLEL_LINK_JOBS={}".format(max_link_jobs)]
            if args.with_c2rust_clang:
                cargs.extend(["-DLLVM_DIR={}/lib/cmake/llvm".format(c.LLVM_BLD),
                              "-DClang_DIR={}/lib/cmake/clang".format(c.LLVM_BLD),
                              "-DLLVM_EXTERNAL_LIT={}/bin/llvm-lit".format(c.LLVM_BLD)])
            else:
                # Some distros, e.g., Arch, Ubuntu, ship llvm-lit as /usr/bin/lit
                cargs.append("-DLLVM_EXTERNAL_LIT={}".format(pb.local['lit']))
            invoke(cmake[cargs])
        else:
            logging.debug("found existing ninja.build, not running cmake")
        invoke(ninja)


def _parse_args():
    """
    define and parse command line arguments here.
    """
    desc = 'build the cross-checkers.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('-c', '--clean-all', default=False,
                        action='store_true', dest='clean_all',
                        help='clean everything before building')
    dhelp = 'build in debug mode (default build is release+asserts)'
    parser.add_argument('-d', '--debug', default=False,
                        action='store_true', dest='debug',
                        help=dhelp)
    lhelp = 'build clang plugin against our C2Rust clang instead of the system one'
    parser.add_argument('--with-c2rust-clang', default=False,
                        action='store_true', dest='with_c2rust_clang',
                        help=lhelp)
    return parser.parse_args()


def _main():
    if on_mac():
        die("Cross-checking is only supported on Linux hosts.")

    setup_logging()
    logging.debug("args: %s", " ".join(sys.argv))

    # earlier plumbum versions are missing features such as TEE
    if pb.__version__ < c.MIN_PLUMBUM_VERSION:
        err = "locally installed version {} of plumbum is too old.\n" \
            .format(pb.__version__)
        err += "please upgrade plumbum to version {} or later." \
            .format(c.MIN_PLUMBUM_VERSION)
        die(err)

    args = _parse_args()
    if args.clean_all:
        logging.info("cleaning all dependencies and previous built files")
        shutil.rmtree(c.CLANG_XCHECK_PLUGIN_BLD, ignore_errors=True)
        make = get_cmd_or_die('make')
        with pb.local.cwd(c.LIBFAKECHECKS_DIR):
            make('clean')

    # prerequisites
    if not have_rust_toolchain(c.CUSTOM_RUST_NAME):
        die("missing rust toolchain: " + c.CUSTOM_RUST_NAME, errno.ENOENT)

    # clang 3.6.0 is known to work; 3.4.0 known to not work.
    ensure_clang_version([3, 6, 0])
    # NOTE: it seems safe to disable this check since we now
    # that we use a rust-toolchain file for rustc versioning.
    # ensure_rustc_version(c.CUSTOM_RUST_RUSTC_VERSION)

    ensure_dir(c.CLANG_XCHECK_PLUGIN_BLD)
    ensure_dir(c.DEPS_DIR)
    git_ignore_dir(c.DEPS_DIR)

    build_clang_plugin(args)


if __name__ == "__main__":
    _main()
