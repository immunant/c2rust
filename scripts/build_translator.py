#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import re
import sys
import json
import errno
import shutil
import logging
import argparse
from typing import Optional

from common import (
    config as c,
    pb,
    get_cmd_or_die,
    download_archive,
    die,
    est_parallel_link_jobs,
    invoke,
    install_sig,
    ensure_dir,
    on_mac,
    setup_logging,
    have_rust_toolchain,
    ensure_clang_version,
    git_ignore_dir,
    on_linux,
    get_ninja_build_type,
)


def download_llvm_sources():
    tar = get_cmd_or_die("tar")

    # make sure we have the gpg public key installed first
    install_sig(c.LLVM_PUBKEY)

    with pb.local.cwd(c.DEPS_DIR):
        # download archives and signatures
        for (aurl, asig, afile, _) in zip(
                c.LLVM_ARCHIVE_URLS,
                c.LLVM_SIGNATURE_URLS,
                c.LLVM_ARCHIVE_FILES,
                c.LLVM_ARCHIVE_DIRS):

            # download archive + signature
            download_archive(aurl, afile, asig)

    # first extract llvm archive
    if not os.path.isdir(c.LLVM_SRC):
        logging.info("extracting %s", c.LLVM_ARCHIVE_FILES[0])
        tar("xf", c.LLVM_ARCHIVE_FILES[0])
        os.rename(c.LLVM_ARCHIVE_DIRS[0], c.LLVM_SRC)

    # then clang front end
    with pb.local.cwd(os.path.join(c.LLVM_SRC, "tools")):
        if not os.path.isdir("clang"):
            logging.info("extracting %s", c.LLVM_ARCHIVE_FILES[1])
            tar("xf", os.path.join(c.ROOT_DIR, c.LLVM_ARCHIVE_FILES[1]))
            os.rename(c.LLVM_ARCHIVE_DIRS[1], "clang")

        with pb.local.cwd("clang/tools"):
            if not os.path.isdir("extra"):
                logging.info("extracting %s", c.LLVM_ARCHIVE_FILES[2])
                tar("xf", os.path.join(c.ROOT_DIR, c.LLVM_ARCHIVE_FILES[2]))
                os.rename(c.LLVM_ARCHIVE_DIRS[2], "extra")


def configure_and_build_llvm(args: str) -> None:
    """
    run cmake as needed to generate ninja buildfiles. then run ninja.
    """
    ninja = get_cmd_or_die("ninja")
    # Possible values are Release, Debug, RelWithDebInfo and MinSizeRel
    build_type = "Debug" if args.debug else "RelWithDebInfo"
    ninja_build_file = os.path.join(c.LLVM_BLD, "build.ninja")
    with pb.local.cwd(c.LLVM_BLD):
        if os.path.isfile(ninja_build_file):
            prev_build_type = get_ninja_build_type(ninja_build_file)
            run_cmake = prev_build_type != build_type
        else:
            run_cmake = True

        if run_cmake:
            cmake = get_cmd_or_die("cmake")
            max_link_jobs = est_parallel_link_jobs()
            assertions = "1" if args.assertions else "0"
            cargs = ["-G", "Ninja", c.LLVM_SRC,
                     "-Wno-dev",
                     "-DCMAKE_C_COMPILER=clang",
                     "-DCMAKE_CXX_COMPILER=clang++",
                     "-DCMAKE_INSTALL_PREFIX=" + c.LLVM_INSTALL,
                     "-DCMAKE_BUILD_TYPE=" + build_type,
                     "-DLLVM_ENABLE_ASSERTIONS=" + assertions,
                     "-DCMAKE_EXPORT_COMPILE_COMMANDS=1",
                     "-DLLVM_TARGETS_TO_BUILD=X86",
                     "-DLLVM_INCLUDE_UTILS=1",
                     "-DLLVM_BUILD_UTILS=1",
                     "-DLLVM_LINK_LLVM_DYLIB=1",
                     "-DLLVM_PARALLEL_LINK_JOBS={}".format(max_link_jobs),
                     "-DTINYCBOR_PREFIX={}".format(c.CBOR_PREFIX)]
            invoke(cmake[cargs])
        else:
            logging.debug("found existing ninja.build, not running cmake")

        ninja_args = ['install']
        invoke(ninja, *ninja_args)


def update_cmakelists(filepath):
    if not os.path.isfile(filepath):
        die("not found: " + filepath, errno.ENOENT)
    indicator = "add_subdirectory(ast-exporter)"

    with open(filepath, "r") as handle:
        cmakelists = handle.readlines()
        add_commands = not any([indicator in l for l in cmakelists])
        logging.debug("add commands to %s: %s", filepath, add_commands)

    if add_commands:
        with open(filepath, "a+") as handle:
            handle.writelines(c.CMAKELISTS_COMMANDS)
        logging.debug("added commands to %s", filepath)


def build_transpiler(debug: bool):
    cargo = get_cmd_or_die("cargo")
    build_flags = ["build"]

    if not debug:
        build_flags.append("--release")

    with pb.local.cwd(os.path.join(c.ROOT_DIR, "transpiler")):
        # use different target dirs for different hosts
        llvm_config = os.path.join(c.LLVM_INSTALL, "bin/llvm-config")
        with pb.local.env(LLVM_CONFIG_PATH=llvm_config):
            # build with custom rust toolchain
            invoke(cargo, "+" + c.CUSTOM_RUST_NAME, *build_flags)


def build_a_bear():
    """
    the output of bear differs between versions, so we build the
    latest bear rather than trying to support multiple versions.
    FIXME: might be better to handle multiple versions instead.
    """
    if os.path.isdir(c.BEAR_PREFIX):
        logging.debug("skipping Bear installation")
        return

    # download
    if not os.path.isfile(c.BEAR_ARCHIVE):
        curl = get_cmd_or_die("curl")
        curl['-s', c.BEAR_URL, '-o', c.BEAR_ARCHIVE] & pb.TEE

    # remove any existing build dir since we don't know if
    # bear was built for the current host environment.
    if os.path.isdir(c.BEAR_SRC):
        shutil.rmtree(c.BEAR_SRC, ignore_errors=True)

    # unpack
    tar = get_cmd_or_die("tar")
    with pb.local.cwd(c.DEPS_DIR):
        tar['xf', c.BEAR_ARCHIVE] & pb.TEE

    # cmake
    bear_build_dir = os.path.join(c.BEAR_SRC, "build")
    bear_install_prefix = "-DCMAKE_INSTALL_PREFIX=" + c.BEAR_PREFIX
    ensure_dir(bear_build_dir)
    with pb.local.cwd(bear_build_dir):
        cmake = get_cmd_or_die("cmake")
        cmake["..", bear_install_prefix] & pb.TEE
        make = get_cmd_or_die("make")
        make["install"] & pb.TEE


def integrate_ast_exporter():
    """
    link ast-exporter into $LLVM_SRC/tools/clang/tools/extra
    """
    abs_src = os.path.join(c.ROOT_DIR, "ast-exporter")
    src = "../../../../../../../ast-exporter"
    exporter_dest = os.path.join(
        c.LLVM_SRC, "tools/clang/tools/extra/ast-exporter")
    clang_tools_extra = os.path.abspath(
        os.path.join(exporter_dest, os.pardir))
    # NOTE: `os.path.exists` returns False on broken symlinks,
    # `lexists` returns True.
    if not os.path.lexists(exporter_dest):
        # NOTE: using os.symlink to emulate `ln -s` would be unwieldy
        ln = get_cmd_or_die("ln")
        with pb.local.cwd(clang_tools_extra):
            ln("-s", src)
    assert os.path.islink(exporter_dest), \
        "missing link: %s->%s" % (src, exporter_dest)
    # check that link points to its intended target
    link_target = os.path.realpath(exporter_dest)
    # print(exporter_dest)
    # print(abs_src)
    assert link_target == abs_src, \
        "invalid link target: %s!=%s" % (link_target, abs_src)

    cmakelists_path = os.path.join(clang_tools_extra, "CMakeLists.txt")
    update_cmakelists(cmakelists_path)


def _parse_args():
    """
    define and parse command line arguments here.
    """
    desc = 'download dependencies for the AST exporter and built it.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('-c', '--clean-all', default=False,
                        action='store_true', dest='clean_all',
                        help='clean everything before building')
    parser.add_argument('--without-assertions', default=True,
                        action='store_false', dest='assertions',
                        help='build the tool and clang without assertions')
    c.add_args(parser)
    args = parser.parse_args()
    c.update_args(args)
    return args


def binary_in_path(binary_name) -> bool:
    try:
        # raises CommandNotFound exception if not available.
        _ = pb.local[binary_name]  # noqa: F841
        return True
    except pb.CommandNotFound:
        return False


def _main():
    setup_logging()
    logging.debug("args: %s", " ".join(sys.argv))

    # FIXME: allow env/cli override of LLVM_SRC, LLVM_VER, and LLVM_BLD
    # FIXME: check that cmake and ninja are installed
    # FIXME: option to build LLVM/Clang from master?

    # earlier plumbum versions are missing features such as TEE
    if pb.__version__ < c.MIN_PLUMBUM_VERSION:
        err = "locally installed version {} of plumbum is too old.\n" \
            .format(pb.__version__)
        err += "please upgrade plumbum to version {} or later." \
            .format(c.MIN_PLUMBUM_VERSION)
        die(err)

    args = _parse_args()

    # prerequisites
    if not have_rust_toolchain(c.CUSTOM_RUST_NAME):
        die("missing rust toolchain: " + c.CUSTOM_RUST_NAME, errno.ENOENT)

    # clang 3.6.0 is known to work; 3.4.0 known to not work.
    ensure_clang_version([3, 6, 0])
    # NOTE: it seems safe to disable this check since we now
    # that we use a rust-toolchain file for rustc versioning.
    # ensure_rustc_version(c.CUSTOM_RUST_RUSTC_VERSION)

    if args.clean_all:
        logging.info("cleaning all dependencies and previous built files")
        shutil.rmtree(c.LLVM_SRC, ignore_errors=True)
        shutil.rmtree(c.LLVM_BLD, ignore_errors=True)
        shutil.rmtree(c.DEPS_DIR, ignore_errors=True)
        cargo = get_cmd_or_die("cargo")
        with pb.local.cwd(os.path.join(c.ROOT_DIR, "transpiler")):
            invoke(cargo, "clean")

    ensure_dir(c.LLVM_BLD)
    ensure_dir(c.DEPS_DIR)
    git_ignore_dir(c.DEPS_DIR)

    if on_linux():
        build_a_bear()
        if not os.path.isfile(c.BEAR_BIN):
            die("bear not found", errno.ENOENT)

    download_llvm_sources()

    # integrate_ast_exporter()

    configure_and_build_llvm(args)

    build_transpiler(args.debug)


if __name__ == "__main__":
    _main()
