#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# This script builds a C2Rust translated version of snudown,
# either with or without cross-checks

from shutil import rmtree
from common import *
import transpile
from collections import namedtuple

MACHINE_NAME = platform.node()
MACHINE_TYPE = platform.platform()

LIB_PATH = get_rust_toolchain_libpath(CUSTOM_RUST_NAME)

C2RUST = ROOT_DIR
AST_EXPORTER = AST_EXPO
AST_IMPORTER = AST_IMPO
RUSTFMT = "rustfmt"

XCHECK_TOPDIR = os.path.join(CROSS_CHECKS_DIR, "rust-checks")
XCHECK_TARGET_DIR = os.path.join(XCHECK_TOPDIR, "target", "debug")
XCHECK_PLUGIN  = os.path.join(XCHECK_TARGET_DIR, "libcross_check_plugin.so")
XCHECK_DERIVE  = os.path.join(XCHECK_TARGET_DIR, "libcross_check_derive.so")
XCHECK_RUNTIME = os.path.join(XCHECK_TARGET_DIR, "libcross_check_runtime.rlib")

# # FIXME: this should be an absolute path, but rustc-plugin cannot handle
# # absolute paths for the external configuration
OUTPUT_DIR = "translator-build"


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

        return outpath


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

    # the macOS and Linux builds of the ast-importer alias each other
    if not is_elf_exe(AST_EXPO) and not on_mac():
        msg = "ast-importer was built for macOS;"
        msg += " please run build_translator.py and retry."
        die(msg)

    generate_html_entries_header(snudown)

    bldr = CompileCommandsBuilder()

    slugs = ["autolink", "buffer", "stack", "markdown"]
    ctmpl = "cc -o {snudown}/src/{slug}.c.o -c {snudown}/src/{slug}.c -Isrc -Ihtml -Wwrite-strings -D_FORTIFY_SOURCE=0 -DNDEBUG=1"

    for s in slugs:
        cmd = ctmpl.format(slug=s, snudown=snudown)
        file = os.path.join(snudown, "src", s + ".c")
        assert os.path.isfile(file), "No such file: " + file
        bldr.add_entry(snudown, cmd, file)

    cmds_json_path = bldr.write_result(os.path.curdir)
    with open(cmds_json_path, "r") as cmds_json:
        impo_args = ["--reloop-cfgs"]
        transpile.transpile_files(cmds_json, multiprocessing.cpu_count(),
                                  extra_impo_args=impo_args,
                                  emit_build_files=True,
                                  cross_checks=xcheck,
                                  cross_check_config=[os.path.join(snudown, "../snudown_rust.c2r")])

    with pb.local.cwd(os.path.join(snudown, "c2rust-build")):
        cargo = get_cmd_or_die("cargo")
        cargo("build")

    logging.info("success!")


USAGE = """\
USAGE:
$ ./translate.py translate <snudown directory>
or
$ ./translate.py rustcheck <snudown directory>
or
$ ./translate.py html_entities <snudown directory>
"""

if __name__ == "__main__":
    # TODO: use argparse package instead?
    if len(sys.argv) < 3 or sys.argv[1] not in ["translate", "rustcheck", "html_entities"]:
        print(USAGE)
        die("missing or invalid argument")

    SNUDOWN = os.path.realpath(sys.argv[2])
    if sys.argv[1] == "html_entities":
        generate_html_entries_header(SNUDOWN)
    else:
        XCHECK = True if sys.argv[1] == "rustcheck" else False
        main(XCHECK, SNUDOWN)

