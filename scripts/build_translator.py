#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import re
import sys
import json
import errno
import shutil
import signal
import logging
import argparse


from common import *
from typing import *


def download_llvm_sources():
    tar = get_cmd_or_die("tar")

    with pb.local.cwd(DEPS_DIR):
        # download archives and signatures
        for (aurl, asig, afile, _) in zip(
                LLVM_ARCHIVE_URLS,
                LLVM_SIGNATURE_URLS,
                LLVM_ARCHIVE_FILES,
                LLVM_ARCHIVE_DIRS):

            # download archive + signature
            download_archive(aurl, afile, asig)

    # first extract llvm archive
    if not os.path.isdir(LLVM_SRC):
        logging.info("extracting %s", LLVM_ARCHIVE_FILES[0])
        tar("xf", LLVM_ARCHIVE_FILES[0])
        os.rename(LLVM_ARCHIVE_DIRS[0], LLVM_SRC)

    # then clang front end
    with pb.local.cwd(os.path.join(LLVM_SRC, "tools")):
        if not os.path.isdir("clang"):
            logging.info("extracting %s", LLVM_ARCHIVE_FILES[1])
            tar("xf", os.path.join(ROOT_DIR, LLVM_ARCHIVE_FILES[1]))
            os.rename(LLVM_ARCHIVE_DIRS[1], "clang")

        with pb.local.cwd("clang/tools"):
            if not os.path.isdir("extra"):
                logging.info("extracting %s", LLVM_ARCHIVE_FILES[2])
                tar("xf", os.path.join(ROOT_DIR, LLVM_ARCHIVE_FILES[2]))
                os.rename(LLVM_ARCHIVE_DIRS[2], "extra")


def get_ninja_build_type(ninja_build_file):
    signature = "# CMAKE generated file: DO NOT EDIT!" + os.linesep
    with open(ninja_build_file, "r") as handle:
        lines = handle.readlines()
        if not lines[0] == signature:
            die("unexpected content in ninja.build: " + ninja_build_file)
        r = re.compile(r'^#\s*Configuration:\s*(\w+)')
        for line in lines:
            m = r.match(line)
            if m:
                # print m.group(1)
                return m.group(1)
        die("missing content in ninja.build: " + ninja_build_file)


def configure_and_build_llvm(args):
    """
    run cmake as needed to generate ninja buildfiles. then run ninja.
    """
    ninja = get_cmd_or_die("ninja")
    # Possible values are Release, Debug, RelWithDebInfo and MinSizeRel
    build_type = "Debug" if args.debug else "RelWithDebInfo"
    ninja_build_file = os.path.join(LLVM_BLD, "build.ninja")
    with pb.local.cwd(LLVM_BLD):
        if os.path.isfile(ninja_build_file):
            prev_build_type = get_ninja_build_type(ninja_build_file)
            run_cmake = prev_build_type != build_type
        else:
            run_cmake = True

        if run_cmake:
            cmake = get_cmd_or_die("cmake")
            invoke(cmake["-G", "Ninja", LLVM_SRC,
                         "-Wno-dev",
                         "-DCMAKE_C_COMPILER=clang",
                         "-DCMAKE_CXX_COMPILER=clang++",
                         "-DCMAKE_C_FLAGS=-I{}/include".format(CBOR_PREFIX),
                         "-DCMAKE_CXX_FLAGS=-I{}/include".format(CBOR_PREFIX),
                         "-DCMAKE_EXE_LINKER_FLAGS=-L{}/lib".format(CBOR_PREFIX),
                         "-DCMAKE_BUILD_TYPE=" + build_type,
                         "-DLLVM_ENABLE_ASSERTIONS=1",
                         "-DLLVM_TARGETS_TO_BUILD=X86"])
        else:
            logging.debug("found existing ninja.build, not running cmake")
        invoke(ninja['ast-extractor'])


def update_cmakelists(filepath):
    if not os.path.isfile(filepath):
        die("not found: " + filepath, errno.ENOENT)
    indicator = "add_subdirectory(ast-extractor)"

    with open(filepath, "r") as handle:
        cmakelists = handle.readlines()
        add_commands = not any([indicator in l for l in cmakelists])
        logging.debug("add commands to %s: %s", filepath, add_commands)

    if add_commands:
        with open(filepath, "a+") as handle:
            handle.writelines(CMAKELISTS_COMMANDS)
        logging.debug("added commands to %s", filepath)


def update_cbor_prefix(makefile):
    """
    rewrite prefix variable in tinycbor makefile.
    """
    if not os.path.isfile(makefile):
        die("not found: " + makefile, errno.ENOENT)

    lines = []
    writeback = False
    with open(makefile, 'r') as fh:
        for line in fh.readlines():
            m = re.match(r'^\s*prefix\s*=\s*([^\s]+)', line)
            if m:
                logging.debug("tinycbor prefix: '%s'", m.group(1))
                prefix = m.group(1)
                writeback = prefix != CBOR_PREFIX
                lines.append("prefix = " + CBOR_PREFIX + os.linesep)
            else:
                lines.append(line)

    if writeback:
        logging.debug("updating tinycbor Makefile")
        with open(makefile, 'w') as fh:
            fh.writelines("".join(lines))


def build_ast_importer():
    cargo = get_cmd_or_die("cargo")

    # assert os.path.isdir(os.path.join(COMPILER_SUBMOD_DIR, 'src'))

    with pb.local.cwd(os.path.join(ROOT_DIR, "ast-importer")):
        # we build with custom rust toolchain here
        invoke(cargo, "+" + CUSTOM_RUST_NAME, "build")


def build_a_bear():
    """
    the output of bear differs between versions, so we build the
    latest bear rather than trying to support multiple versions.
    FIXME: might be better to handle multiple versions instead.
    """
    if os.path.isdir(BEAR_PREFIX):
        logging.debug("skipping Bear installation")
        return

    # download
    if not os.path.isfile(BEAR_ARCHIVE):
        curl = get_cmd_or_die("curl")
        curl['-s', BEAR_URL, '-o', BEAR_ARCHIVE] & pb.TEE

    # remove any existing build dir since we don't know if
    # bear was built for the current host environment.
    if os.path.isdir(BEAR_SRC):
        shutil.rmtree(BEAR_SRC, ignore_errors=True)

    # unpack
    tar = get_cmd_or_die("tar")
    with pb.local.cwd(DEPS_DIR):
        tar['xf', BEAR_ARCHIVE] & pb.TEE

    # cmake
    bear_build_dir = os.path.join(BEAR_SRC, "build")
    bear_install_prefix = "-DCMAKE_INSTALL_PREFIX=" + BEAR_PREFIX
    ensure_dir(bear_build_dir)
    with pb.local.cwd(bear_build_dir):
        cmake = get_cmd_or_die("cmake")
        cmake["..", bear_install_prefix] & pb.TEE
        make = get_cmd_or_die("make")
        make["install"] & pb.TEE


def install_tinycbor() -> Optional[str]:
    """
    download, unpack, build, and install tinycbor.
    """
    def path_to_cc_db():
        cc_cmd_db = os.path.join(CBOR_SRC, "compile_commands.json")
        if not os.path.isfile(cc_cmd_db) and not on_mac():
            die("not found: " + cc_cmd_db)
        return cc_cmd_db

    if os.path.isdir(CBOR_PREFIX) and os.path.isfile(CBOR_ARCHIVE):
        logging.debug("skipping tinycbor installation")
        return path_to_cc_db()

    # download
    if not os.path.isfile(CBOR_ARCHIVE):
        curl = get_cmd_or_die("curl")
        curl['-s', CBOR_URL, '-o', CBOR_ARCHIVE] & pb.TEE

    # remove any existing build dir since we don't know if
    # tinycbor was built for the current host environment.
    if os.path.isdir(CBOR_SRC):
        shutil.rmtree(CBOR_SRC, ignore_errors=True)

    # unpack
    tar = get_cmd_or_die("tar")
    with pb.local.cwd(DEPS_DIR):
        tar['xf', CBOR_ARCHIVE] & pb.TEE

    # update install prefix
    update_cbor_prefix(os.path.join(CBOR_SRC, "Makefile"))

    # make && install
    # NOTE: we use bear to wrap make invocations such that
    # we get a .json database of compiler commands that we
    # can use to test ast-extractor. On macOS, bear requires
    # system integrity protection to be turned off, so we
    # only use bear on Ubuntu Linux hosts.
    with pb.local.cwd(CBOR_SRC):
        make = get_cmd_or_die("make")
        if not on_mac():
            bear = get_cmd_or_die(BEAR_BIN)
            make = bear[make]
        make & pb.TEE  # nopep8
        make('install')  # & pb.TEE

    return path_to_cc_db()


def integrate_ast_extractor():
    """
    link ast-extractor into $LLVM_SRC/tools/clang/tools/extra
    """
    abs_src = os.path.join(ROOT_DIR, "ast-extractor")
    src = "../../../../../../../ast-extractor"
    extractor_dest = os.path.join(
        LLVM_SRC, "tools/clang/tools/extra/ast-extractor")
    clang_tools_extra = os.path.abspath(
        os.path.join(extractor_dest, os.pardir))
    # NOTE: `os.path.exists` returns False on broken symlinks, `lexists` returns True.
    if not os.path.lexists(extractor_dest):
        # NOTE: using os.symlink to emulate `ln -s` would be unwieldy
        ln = get_cmd_or_die("ln")
        with pb.local.cwd(clang_tools_extra):
            ln("-s", src)
    assert os.path.islink(extractor_dest), \
        "missing link: %s->%s" % (src, extractor_dest)
    # check that link points to its intended target
    link_target = os.path.realpath(extractor_dest)
    print(extractor_dest)
    print(abs_src)
    assert link_target == abs_src, \
        "invalid link target: %s!=%s" % (link_target, abs_src)

    cmakelists_path = os.path.join(clang_tools_extra, "CMakeLists.txt")
    update_cmakelists(cmakelists_path)


def _parse_args():
    """
    define and parse command line arguments here.
    """
    desc = 'download dependencies for the AST extractor and built it.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('-c', '--clean-all', default=False,
                        action='store_true', dest='clean_all',
                        help='clean everything before building')
    dhelp = 'build in debug mode (default build is release+asserts)'
    parser.add_argument('-d', '--debug', default=False,
                        action='store_true', dest='debug',
                        help=dhelp)
    thelp = 'sanity test ast extractor using tinycbor (linux only)'
    parser.add_argument('-t', '--test', default=False,
                        action='store_true', dest='sanity_test',
                        help=thelp)
    return parser.parse_args()


def test_ast_extractor(cc_db_path: str):
    """
    run ast-extractor on tinycbor if on linux. testing is
    not supported on macOS since bear requires system integrity
    protection to be disabled.
    """
    assert not on_mac(), "sanity testing requires linux host"

    ast_extr = os.path.join(LLVM_BIN, "ast-extractor")
    if not os.path.isfile(ast_extr):
        die("ast-extractor not found in " + LLVM_BIN)
    ast_extr = get_cmd_or_die(ast_extr)

    include_dirs = get_system_include_dirs()

    with open(cc_db_path, "r") as handle:
        cc_db = json.load(handle)
    cbor_files = [extract_ast_from(ast_extr, cc_db_path, include_dirs, **cmd)
                  for cmd in cc_db]

    logging.info("PASS sanity testing")


def binary_in_path(binary_name) -> bool:
    try:
        # raises CommandNotFound exception if not available.
        _ = pb.local[binary_name]
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
    if pb.__version__ < MIN_PLUMBUM_VERSION:
        err = "locally installed version {} of plumbum is too old.\n" \
            .format(pb.__version__)
        err += "please upgrade plumbum to version {} or later." \
            .format(MIN_PLUMBUM_VERSION)
        die(err)

    args = _parse_args()
    if args.clean_all:
        logging.info("cleaning all dependencies and previous built files")
        shutil.rmtree(LLVM_SRC, ignore_errors=True)
        shutil.rmtree(LLVM_BLD, ignore_errors=True)
        shutil.rmtree(DEPS_DIR, ignore_errors=True)

    # prerequisites
    if not have_rust_toolchain(CUSTOM_RUST_NAME):
        die("missing rust toolchain: " + CUSTOM_RUST_NAME, errno.ENOENT)

    # clang 3.6.0 is known to work; 3.4.0 known to not work.
    ensure_clang_version([3, 6, 0])
    ensure_rustc_version(CUSTOM_RUST_RUSTC_VERSION)

    ensure_dir(LLVM_BLD)
    ensure_dir(DEPS_DIR)

    if on_linux():
        build_a_bear()
        if not os.path.isfile(BEAR_BIN):
            die("bear not found", errno.ENOENT)

    download_llvm_sources()

    integrate_ast_extractor()

    cc_db = install_tinycbor()

    configure_and_build_llvm(args)

    # NOTE: we're not doing this anymore since it is
    # faster and takes less space to simply pull the
    # prebuilt nightly binaries with rustup
    # download_and_build_custom_rustc(args)

    build_ast_importer()

    if not on_mac() and args.sanity_test:
        test_ast_extractor(cc_db)


if __name__ == "__main__":
    _main()
