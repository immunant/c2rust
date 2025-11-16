#!/usr/bin/env -S uv run
# -*- coding: utf-8 -*-

import os
import sys
import shutil
import logging
import argparse

from common import (
    config as c,
    pb,
    get_cmd_or_die,
    download_archive,
    die,
    est_parallel_link_jobs,
    invoke,
    invoke_quietly,
    install_sig,
    ensure_dir,
    on_mac,
    setup_logging,
    git_ignore_dir,
    get_ninja_build_type,
)


def download_llvm_sources() -> None:
    tar = get_cmd_or_die("tar")

    if not c.LLVM_SKIP_SIGNATURE_CHECKS:
        # make sure we have the gpg public key installed first
        install_sig(c.LLVM_PUBKEY)

    assert c.LLVM_ARCHIVE_URLS is not None  # for mypy

    with pb.local.cwd(c.BUILD_DIR):
        # download archives and signatures
        for (aurl, asig, afile, _) in zip(
                c.LLVM_ARCHIVE_URLS,
                c.LLVM_SIGNATURE_URLS,
                c.LLVM_ARCHIVE_FILES,
                c.LLVM_ARCHIVE_DIRS):

            if c.LLVM_SKIP_SIGNATURE_CHECKS:
                asig = None  # type: ignore
            # download archive and (by default) its signature
            download_archive(aurl, afile, asig)

    # first extract llvm archive,
    if not os.path.isdir(c.LLVM_SRC):
        logging.info("extracting %s", c.LLVM_ARCHIVE_FILES[0])
        tar("xf", c.LLVM_ARCHIVE_FILES[0])
        os.rename(c.LLVM_ARCHIVE_DIRS[0], c.LLVM_SRC)

    # then compiler-rt,
    with pb.local.cwd(os.path.join(c.LLVM_SRC, "projects")):
        if not os.path.isdir("compiler-rt"):
            logging.info("extracting %s", c.LLVM_ARCHIVE_FILES[2])
            tar("xf", os.path.join(c.ROOT_DIR, c.LLVM_ARCHIVE_FILES[2]))
            os.rename(c.LLVM_ARCHIVE_DIRS[2], "compiler-rt")

    # then clang
    with pb.local.cwd(os.path.join(c.LLVM_SRC, "tools")):
        if not os.path.isdir("clang"):
            logging.info("extracting %s", c.LLVM_ARCHIVE_FILES[1])
            tar("xf", os.path.join(c.ROOT_DIR, c.LLVM_ARCHIVE_FILES[1]))
            os.rename(c.LLVM_ARCHIVE_DIRS[1], "clang")

    (major, _minor, _point) = c.LLVM_VER.split(".")
    major = int(major)

    # finally cmake files if we're building LLVM 15 or later
    if len(c.LLVM_ARCHIVE_FILES) == 4:
        assert os.path.basename(c.LLVM_ARCHIVE_FILES[3]).startswith("cmake")
        with pb.local.cwd(os.path.join(c.LLVM_SRC, "cmake", "modules")):
            logging.info("extracting %s", c.LLVM_ARCHIVE_FILES[3])
            cmake_modules_dir = os.path.join(c.LLVM_SRC, "cmake", "modules")
            # extract *.cmake files into llvm/cmake/modules
            tar("xf", c.LLVM_ARCHIVE_FILES[3],
                "--strip-components=2",
                "--directory", cmake_modules_dir)

        if major > 15:
            # workaround for https://stackoverflow.com/questions/75787113
            cmake_symlink_dir = os.path.join(c.LLVM_SRC, os.pardir, "cmake",
                                                "Modules")
            cmake_symlink_dir = os.path.abspath(cmake_symlink_dir)
            ensure_dir(os.path.join(c.LLVM_SRC, os.pardir, "cmake"))
            if not os.path.exists(cmake_symlink_dir):
                os.symlink(cmake_modules_dir, cmake_symlink_dir)

        if major >= 17:
            # similar to above workaround but for compiler-rt
            cmake_symlink_dir = os.path.join(c.LLVM_SRC, "projects", "cmake",
                                                "Modules")
            ensure_dir(os.path.join(c.LLVM_SRC, "projects", "cmake"))
            if not os.path.exists(cmake_symlink_dir):
                os.symlink(cmake_modules_dir, cmake_symlink_dir)


def configure_and_build_llvm(args: argparse.Namespace) -> None:
    """
    run cmake as needed to generate ninja buildfiles. then run ninja.
    """
    # Possible values are Release, Debug, RelWithDebInfo and MinSizeRel
    build_type = "Debug" if args.debug else "RelWithDebInfo"
    ninja_build_file = os.path.join(c.LLVM_BLD, "build.ninja")
    with pb.local.cwd(c.LLVM_BLD):
        cmake = get_cmd_or_die("cmake")
        max_link_jobs = est_parallel_link_jobs()
        assertions = "1" if args.assertions else "0"
        cargs = ["-G", "Ninja", c.LLVM_SRC,
                    "-Wno-dev",
                    "-DLLVM_ENABLE_ZSTD=0",
                    "-DLLVM_INCLUDE_TESTS=0",
                    "-DCOMPILER_RT_INCLUDE_TESTS=0",
                    "-DCMAKE_INSTALL_PREFIX=" + c.LLVM_INSTALL,
                    "-DCMAKE_BUILD_TYPE=" + build_type,
                    "-DLLVM_PARALLEL_LINK_JOBS={}".format(max_link_jobs),
                    "-DLLVM_ENABLE_ASSERTIONS=" + assertions,
                    "-DCMAKE_EXPORT_COMPILE_COMMANDS=1",
                    "-DLLVM_TARGETS_TO_BUILD=host",
                    "-DLLVM_INCLUDE_BENCHMARKS=0",
                    "-DCOMPILER_RT_ENABLE_IOS=OFF",
        ]

        invoke(cmake[cargs])

        # We must install headers here so our clang tool can reference
        # compiler-internal headers such as stddef.h. This reference is
        # relative to LLVM_INSTALL/bin, which MUST exist for the relative
        # reference to be valid. To force this, we also install llvm-config,
        # since we are building and using it for other purposes.
        nice = get_cmd_or_die("nice")
        ninja = get_cmd_or_die("ninja")
        nice_args = [
            '-n', '19', str(ninja),
            'clangAST',
            'clangFrontend',
            'clangTooling',
            'clangBasic',
            'clangASTMatchers',
            'clangParse',
            'clangSerialization',
            'clangSema',
            'clangEdit',
            'clangAnalysis',
            'clangDriver',
            'clangFormat',
            'clangToolingCore',
            'clangRewrite',
            'clangLex',
            'LLVMMC',
            'LLVMMCParser',
            'LLVMDemangle',
            'LLVMSupport',
            'LLVMOption',
            'LLVMBinaryFormat',
            'LLVMCore',
            'LLVMBitReader',
            'LLVMProfileData',
            'llvm-config',
            'install-clang-headers', 'install-compiler-rt-headers',
            'FileCheck', 'count', 'not']
        (major_str, _minor, _point) = c.LLVM_VER.split(".")
        major = int(major_str)
        if major >= 7 and major < 10:
            nice_args += [
                'LLVMDebugInfoMSF',
                'LLVMDebugInfoCodeView']
        if major > 8:
            nice_args.append("install-clang-resource-headers")
        if major == 9:
            nice_args += [
                'LLVMBitstreamReader',
                'LLVMRemarks']
        if major >= 10:
            nice_args.append("LLVMFrontendOpenMP")
        if args.with_clang:
            nice_args.append('clang')
        invoke(nice, *nice_args)

        # Make sure install/bin exists so that we can create a relative path
        # using it in AstExporter.cpp
        os.makedirs(os.path.join(c.LLVM_INSTALL, 'bin'), exist_ok=True)


def need_cargo_clean(args: argparse.Namespace) -> bool:
    """
    Cargo may not pick up changes in c.BUILD_DIR that would require
    a rebuild. This function tries to detect when we need to clean.
    """
    c2rust = c2rust_bin_path(args)
    if not os.path.isfile(c2rust):
        logging.debug("need_cargo_clean:False:no-c2rust-bin")
        return False

    find = get_cmd_or_die("find")
    _retcode, stdout, _ = invoke_quietly(find, c.BUILD_DIR, "-cnewer", c2rust)
    include_pattern = "install/lib/clang/{ver}/include".format(ver=c.LLVM_VER)
    for line in stdout.split("\n")[:-1]:  # skip empty last line
        if line.endswith("install_manifest_clang-headers.txt") or \
                line.endswith("ninja_log") or \
                include_pattern in line:
            continue
        else:
            logging.debug("need_cargo_clean:True:%s", line)
            return True
    logging.debug("need_cargo_clean:False")
    return False


def build_transpiler(args: argparse.Namespace) -> None:
    nice = get_cmd_or_die("nice")
    cargo = get_cmd_or_die("cargo")

    if need_cargo_clean(args):
        invoke(cargo, "clean")

    build_flags = ["-n", "19", str(cargo), "build", "--features", "llvm-static"]

    if not args.debug:
        build_flags.append("--release")

    if args.verbose:
        build_flags.append("-vv")

    llvm_config = os.path.join(c.LLVM_BLD, "bin/llvm-config")
    assert os.path.isfile(llvm_config), \
        "expected llvm_config at " + llvm_config

    if on_mac():
        llvm_system_libs = "-lz -lcurses -lm -lxml2"
    else:  # linux
        llvm_system_libs = "-lz -lrt -ltinfo -ldl -lpthread -lm"

    llvm_libdir = os.path.join(c.LLVM_BLD, "lib")

    # NOTE: the `curl-rust` and `libz-sys` crates use the `pkg_config`
    # crate to locate the system libraries they wrap. This causes
    # `pkg_config` to add `/usr/lib` to `rustc`s library search path
    # which means that our `cargo` invocation picks up the system
    # libraries even when we're trying to link against libs we built.
    # https://docs.rs/pkg-config/0.3.14/pkg_config/
    with pb.local.cwd(c.C2RUST_DIR):
        with pb.local.env(LIBCURL_NO_PKG_CONFIG=1,
                          ZLIB_NO_PKG_CONFIG=1,
                          LLVM_CONFIG_PATH=llvm_config,
                          LLVM_LIB_DIR=llvm_libdir,
                          LLVM_SYSTEM_LIBS=llvm_system_libs):
            invoke(nice, *build_flags)


def _parse_args() -> argparse.Namespace:
    """
    define and parse command line arguments here.
    """
    desc = 'download dependencies for the AST exporter and built it.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('-c', '--clean-all', default=False,
                        action='store_true', dest='clean_all',
                        help='clean everything before building')
    parser.add_argument('--with-clang', default=False,
                        action='store_true', dest='with_clang',
                        help='build clang with this tool')
    llvm_ver_help = 'fetch and build specified version of clang/LLVM (default: {})'.format(c.LLVM_VER)
    # FIXME: build this list by globbing for scripts/llvm-*.0.*-key.asc
    llvm_ver_choices = ["6.0.0", "6.0.1", "7.0.0", "7.0.1", "8.0.0", "9.0.0",
        "10.0.0", "10.0.1", "11.0.0", "11.1.0", "12.0.0", "15.0.1", "16.0.6",
        "17.0.6", "18.1.1"]
    parser.add_argument('--with-llvm-version', default=None,
                        action='store', dest='llvm_ver',
                        help=llvm_ver_help, choices=llvm_ver_choices)
    parser.add_argument('--without-assertions', default=True,
                        action='store_false', dest='assertions',
                        help='build the tool and clang without assertions')
    parser.add_argument('-v', '--verbose', default=False,
                        action='store_true', dest='verbose',
                        help='emit verbose information during build')
    parser.add_argument('--skip-signature-checks', default=False,
                        action='store_true', dest='llvm_skip_signature_checks',
                        help='skip signature check of source code archives')

    c.add_args(parser)
    args = parser.parse_args()

    c.update_args(args)
    return args


def binary_in_path(binary_name: str) -> bool:
    try:
        # raises CommandNotFound exception if not available.
        _ = pb.local[binary_name]  # noqa: F841
        return True
    except pb.CommandNotFound:
        return False


def c2rust_bin_path(args: argparse.Namespace) -> str:
    c2rust_bin_path = 'target/debug/c2rust' if args.debug \
                      else 'target/release/c2rust'
    c2rust_bin_path = os.path.join(c.ROOT_DIR, c2rust_bin_path)

    abs_curdir = os.path.abspath(os.path.curdir)
    return os.path.relpath(c2rust_bin_path, abs_curdir)


def print_success_msg(args: argparse.Namespace) -> None:
    """
    print a helpful message on how to run the c2rust binary.
    """
    print("success! you may now run", c2rust_bin_path(args))


def _main() -> None:
    setup_logging()
    logging.debug("args: %s", " ".join(sys.argv))

    # FIXME: allow env/cli override of LLVM_SRC and LLVM_BLD
    # FIXME: check that cmake and ninja are installed
    # FIXME: option to build LLVM/Clang from master?

    args = _parse_args()

    if args.clean_all:
        logging.info("cleaning all dependencies and previous built files")
        shutil.rmtree(c.LLVM_SRC, ignore_errors=True)
        shutil.rmtree(c.LLVM_BLD, ignore_errors=True)
        shutil.rmtree(c.BUILD_DIR, ignore_errors=True)
        shutil.rmtree(c.AST_EXPO_PRJ_DIR, ignore_errors=True)
        cargo = get_cmd_or_die("cargo")
        with pb.local.cwd(c.ROOT_DIR):
            invoke(cargo, "clean")

    ensure_dir(c.LLVM_BLD)
    ensure_dir(c.BUILD_DIR)
    git_ignore_dir(c.BUILD_DIR)

    download_llvm_sources()
    configure_and_build_llvm(args)
    build_transpiler(args)
    print_success_msg(args)


if __name__ == "__main__":
    _main()
