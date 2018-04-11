#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# This script builds a C2Rust translated version of snudown,
# either with or without cross-checks
#
# Usage:
# $ ./translate.sh translate
# or
# $ ./translate.sh rustcheck

from shutil import rmtree
from common import *
from collections import namedtuple

MACHINE_NAME = platform.node()
MACHINE_TYPE = platform.platform()

LIB_PATH = get_rust_toolchain_libpath(CUSTOM_RUST_NAME)

C2RUST = ROOT_DIR
AST_EXTRACTOR = AST_EXTR
AST_IMPORTER = AST_IMPO
RUSTFMT = "rustfmt"

XCHECK_TOPDIR = os.path.join(CROSS_CHECKS_DIR, "rust-checks")
XCHECK_PLUGIN = os.path.join(XCHECK_TOPDIR, "rustc-plugin/target/debug/libcross_check_plugin.so")
XCHECK_DERIVE = os.path.join(XCHECK_TOPDIR, "derive-macros/target/debug/libcross_check_derive.so")
XCHECK_RUNTIME = os.path.join(XCHECK_TOPDIR, "runtime/target/debug/libcross_check_runtime.rlib")

# # FIXME: this should be an absolute path, but rustc-plugin cannot handle
# # absolute paths for the external configuration
OUTPUT_DIR = "translator-build"


def translate(slug: str, xcheck: bool, snudown: str) -> None:
    """
    :param slug: file name without directory or suffix
    :param xcheck: insert cross checking code
    """
    ast_extr = get_cmd_or_die(AST_EXTR)
    ast_impo = get_cmd_or_die(AST_IMPO)

    # extraction step
    c_src_path = os.path.join(snudown, "src/{}.c".format(slug))
    ast_extr(c_src_path)

    # importer step
    rust_src_path = os.path.join(OUTPUT_DIR, "{}.rs".format(slug))
    with pb.local.env(RUST_BACKTRACE=1,
                      LD_LIBRARY_PATH=LIB_PATH):
        cbor_path = c_src_path + ".cbor"
        logging.debug("importing %s", cbor_path)
        args = ['--reloop-cfgs', cbor_path]
        if xcheck:
            args += ['--cross-checks',
                     '--cross-check-config', os.path.join(snudown, "../snudown_rust.c2r")]
        stdout = ast_impo(*args)
        logging.debug("job's done")
        with open(rust_src_path, "w") as rust_fh:
            rust_fh.writelines(stdout)
        logging.debug("wrote rust output to %s", rust_src_path)

    # formatting step
    # logging.debug("formatting %s", rust_src_path)
    # rustfmt = _get_tool_from_rustup("rustfmt")
    # rustfmt[rust_src_path, '--force'] & pb.TEE(retcode=None)

    # compilation step
    rust_bin_path = os.path.join(OUTPUT_DIR, "lib{}.rlib".format(slug))
    logging.debug("compiling %s -> %s", rust_src_path, rust_bin_path)
    rustc = get_cmd_from_rustup("rustc")
    args = ['--crate-type=rlib',
            '--crate-name=' + slug,
            rust_src_path,
            '-o', rust_bin_path,
            '-g']
    if xcheck:
        args += ['--extern', 'cross_check_plugin=' + XCHECK_PLUGIN,
                 '--extern', 'cross_check_derive=' + XCHECK_DERIVE,
                 '--extern', 'cross_check_runtime=' + XCHECK_RUNTIME]
    rustc(*args)


CompileCommand = namedtuple('CompileCommand', ['directory', 'command', 'file'])


class CompileCommandsBuilder(object):
    entries: List[CompileCommand] = []

    def add_entry(self, dir: str, cmd: str, file: str) -> None:
        entry = CompileCommand(directory=dir, command=cmd, file=file)
        self.entries.append(entry._asdict())

    def write_result(self, outdir: str) -> None:
        assert os.path.isdir(outdir), "No such dir: " + outdir
        outpath = os.path.join(outdir, CC_DB_JSON)
        outjson = json.dumps(self.entries, indent=2)
        # print(outjson)
        with open(outpath, "w") as ccdb_fh:
            ccdb_fh.writelines(outjson)


def generate_html_entries_header(snudown: str):
    """
    Generate html_entities.h from html_entities.gperf
    """
    if not os.path.isfile(os.path.join(snudown, "src/html_entities.h")):
        with pb.local.cwd(os.path.join(snudown, "src")):
            gperf = get_cmd_or_die("gperf")
            gperf('html_entities.gperf', '--output-file=html_entities.h')


def main(xcheck: bool, snudown: str):
    setup_logging()

    if os.path.isdir(OUTPUT_DIR):
        logging.debug("removing existing output dir %s", OUTPUT_DIR)
        rmtree(OUTPUT_DIR)
    os.mkdir(OUTPUT_DIR)

    # make sure that we built the cross-checking libraries if cross checking
    if xcheck:
        bins = [XCHECK_PLUGIN, XCHECK_DERIVE, XCHECK_RUNTIME]
        for b in bins:
            if not os.path.isfile(b):
                msg = "missing binary:\n\t{}\n\nrun `cargo build` to compile it and retry."
                msg = msg.format(b)
                die(msg)

    # make sure the snudown submodule is checked out and up to date
    # update_or_init_submodule(snudown)

    # the macOS and Linux builds of the ast-extractor alias each other
    if not is_elf_exe(AST_EXTR) and not on_mac():
        msg = "ast-importer was built for macOS;"
        msg += " please run build_translator.py and retry."
        die(msg)

    generate_html_entries_header(snudown)

    bldr = CompileCommandsBuilder()

    slugs = ["autolink", "buffer", "stack", "markdown"]
    ctmpl = "cc -o {odir}/{slug}.c.o -c {snudown}/src/{slug}.c -Wwrite-strings -D_FORTIFY_SOURCE=0 -DNDEBUG=1"

    for s in slugs:
        cmd = ctmpl.format(odir=OUTPUT_DIR, slug=s, snudown=snudown)
        file = os.path.join(snudown, "src", s + ".c")
        assert os.path.isfile(file), "No such file: " + file
        bldr.add_entry(snudown, cmd, file)

    bldr.write_result(os.path.curdir)

    for s in slugs:
        translate(s, xcheck, snudown)

    rustc = get_cmd_from_rustup("rustc")
    args = ['--crate-type=staticlib',
            '--crate-name=snudownrust',
            '-L', OUTPUT_DIR,
            os.path.join(C2RUST, "examples/snudown/snudownrust.rs")]

    if xcheck:
        args += ['--extern', 'cross_check_derive=' + XCHECK_DERIVE]
        args += ['--extern', 'cross_check_runtime=' + XCHECK_RUNTIME]
        args += ['--cfg', 'feature=\"cross-check\"']
        args += ['-o', os.path.join(OUTPUT_DIR, "libsnudownrustxcheck.a")]
    else:
        args += ['-o', os.path.join(OUTPUT_DIR, "libsnudownrust.a")]
    rustc(*args)

    logging.info("success!")


USAGE = """\
USAGE:
$ ./translate.sh translate <snudown directory>
or
$ ./translate.sh rustcheck <snudown directory>
"""

if __name__ == "__main__":
    # TODO: use argparse package instead?
    if len(sys.argv) < 3 or sys.argv[1] not in ["translate", "rustcheck"]:
        print(USAGE)
        die("missing or invalid argument")

    XCHECK = True if sys.argv[1] == "rustcheck" else False
    SNUDOWN = os.path.realpath(sys.argv[2])
    main(XCHECK, SNUDOWN)





