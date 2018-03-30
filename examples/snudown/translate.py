#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# This file builds a C2Rust translated version of snudown,
# either with or without cross-checks
#
# Usage:
# $ ./translate.sh translate
# or
# $ ./translate.sh rustcheck

from common import *
from collections import namedtuple
Command = pb.machines.LocalCommand

MACHINE_NAME = platform.node()
MACHINE_TYPE = platform.platform()

LIB_PATH = get_rust_toolchain_libpath(CUSTOM_RUST_NAME)

C2RUST = ROOT_DIR
SNUDOWN = os.path.join(EXAMPLES_DIR, "snudown/repo")
AST_EXTRACTOR=AST_EXTR
AST_IMPORTER=AST_IMPO
RUSTFMT="rustfmt"

XCHECK_TOPDIR=os.path.join(CROSS_CHECKS_DIR, "rust-check")
# XCHECK_PLUGIN=$XCHECK_TOPDIR/rustc-plugin/target/debug/libcross_check_plugin.so
XCHECK_PLUGIN=os.path.join(XCHECK_TOPDIR, "rustc-plugin/target/debug/libcross_check_plugin.so")
# XCHECK_DERIVE=$XCHECK_TOPDIR/derive-macros/target/debug/libcross_check_derive.so
XCHECK_DERIVE=os.path.join(XCHECK_TOPDIR, "derive-macros/target/debug/libcross_check_derive.so")
# XCHECK_RUNTIME=$XCHECK_TOPDIR/runtime/target/debug/libcross_check_runtime.rlib
XCHECK_RUNTIME=os.path.join(XCHECK_TOPDIR, "runtime/target/debug/libcross_check_runtime.rlib")

# # FIXME: this should be an absolute path, but rustc-plugin cannot handle
# # absolute paths for the external configuration
# #OUTPUT_DIR=$SNUDOWN/translator-build
# OUTPUT_DIR=translator-build
OUTPUT_DIR="translator-build"


def _get_tool_from_rustup(toolname: str) -> Command:
    rustup = get_cmd_or_die("rustup")
    toolpath: str = rustup('run', CUSTOM_RUST_NAME, 'which', toolname).strip()
    return pb.local.get(toolpath)

# translate() {
#   $AST_EXTRACTOR $SNUDOWN/src/$1.c
#   env RUST_BACKTRACE=1 LD_LIBRARY_PATH=$LIB_PATH $AST_IMPORTER --reloop-cfgs $SNUDOWN/src/$1.c.cbor > $OUTPUT_DIR/$1.rs
#   #$RUSTFMT $OUTPUT_DIR/$1.rs --force
#   rustc --crate-type=rlib --crate-name=$1 $OUTPUT_DIR/$1.rs -o $OUTPUT_DIR/lib$1.rlib
# }


def translate(slug: str):
    ast_extr = get_cmd_or_die(AST_EXTR)
    ast_impo = get_cmd_or_die(AST_IMPO)

    # extraction step
    c_src_path = os.path.join(SNUDOWN, "src/{}.c".format(slug))
    ast_extr(c_src_path)

    # importer step
    rust_src_path: str = os.path.join(OUTPUT_DIR, "{}.rs".format(slug))
    with pb.local.env(RUST_BACKTRACE=1,
                      LD_LIBRARY_PATH=LIB_PATH):
        cbor_path = c_src_path + ".cbor"
        logging.debug("importing %s", cbor_path)
        stdout = ast_impo('--reloop-cfgs', cbor_path)
        logging.debug("job's done")
        with open(rust_src_path, "w") as rust_fh:
            rust_fh.writelines(stdout)
        logging.debug("wrote rust output to %s", rust_src_path)

    # formatting step
    logging.debug("formatting %s", rust_src_path)
    rustfmt = _get_tool_from_rustup("rustfmt")
    # rustfmt(rust_src_path, '--force')

    # compilation step
    rust_bin_path: str = os.path.join(OUTPUT_DIR, "{}.rlib".format(slug))
    logging.debug("compiling %s -> %s", rust_src_path, rust_bin_path)
    rustc = _get_tool_from_rustup("rustc")
    rustc('--crate-type=rlib', '--crate-name=' + slug, rust_src_path, '-o', rust_bin_path)


# translate_xcheck() {
#   $AST_EXTRACTOR $SNUDOWN/src/$1.c
#   env RUST_BACKTRACE=1 LD_LIBRARY_PATH=$LIB_PATH \
#       $AST_IMPORTER --reloop-cfgs --cross-checks \
#       --cross-check-config $SNUDOWN/../snudown_rust.c2r \
#       -- $SNUDOWN/src/$1.c.cbor > $OUTPUT_DIR/$1.rs
#   #$RUSTFMT $OUTPUT_DIR/$1.rs --force
#   rustc -g --crate-type=rlib --crate-name=$1 \
#       --extern cross_check_plugin=$XCHECK_PLUGIN \
#       --extern cross_check_derive=$XCHECK_DERIVE \
#       --extern cross_check_runtime=$XCHECK_RUNTIME \
#       $OUTPUT_DIR/$1.rs -o $OUTPUT_DIR/lib$1.rlib \
#       #--Z unstable-options --pretty=expanded \
# }

# def translate(src: str):
#     ast_extr = get_cmd_or_die(AST_EXTR)
#     args = ['--reloop-cfgs', '--cross-checks', '--cross-check-config',
#             ]
#     with pb.local.env(RUST_BACKTRACE=1,
#                       LD_LIBRARY_PATH=LIB_PATH):
#         invoke(ast_extr, args)

# compile_commands_entry() {

#         cat >> compile_commands.json <<END
# {
#   "directory": "${SNUDOWN}",
#   "command": "cc -o ${OUTPUT_DIR}/${1}.c.o -c ${SNUDOWN}/src/${1}.c -Wwrite-strings -D_FORTIFY_SOURCE=0 -DNDEBUG=1",
#   "file": "${SNUDOWN}/src/${1}.c"
# },
# END

# }


CompileCommand = namedtuple('CompileCommand', ['directory', 'command', 'file'])


class CCDBBuilder(object):

    entries: List[CompileCommand] = []

    def add_entry(self, dir: str, cmd: str, file: str) -> None:
        entry = CompileCommand(directory=dir, command=cmd, file=file)
        self.entries.append(entry._asdict())

    def write_result(self, outdir: str) -> None:
        assert os.path.isdir(outdir), "No such dir: " + outdir
        outpath: str = os.path.join(outdir, CC_DB_JSON)
        outjson: str = json.dumps(self.entries, indent=2)
        # print(outjson)
        with open(outpath, "w") as ccdb_fh:
            ccdb_fh.writelines(outjson)


if __name__ == "__main__":

    setup_logging()
    logging.debug(LIB_PATH)

    git = get_cmd_or_die("git")
    invoke_quietly(git, "submodule", "update", "--init", SNUDOWN)
    logging.debug("updated submodule %s", SNUDOWN)

    # the macOS and Linux builds of the ast-extractor alias each other
    if not is_elf_exe(AST_EXTR) and not on_mac():
        msg = "ast-importer was built for macOS;"
        msg += " please run build_translator.py and retry."
        die(msg)

    # Generate html_entities.h from html_entities.gpers (setup.py used to do this)
    if not os.path.isfile(os.path.join(SNUDOWN, "src/html_entities.h")):
        with pb.local.cwd(os.path.join(SNUDOWN, "src")):
            gperf = get_cmd_or_die("gperf")
            gperf('html_entities.gperf', '--output-file=html_entities.h')

    if sys.argv[1] == "translate":
        bldr = CCDBBuilder()

        slugs = ["autolink", "buffer", "stack", "markdown"]
        ctmpl = "cc -o {odir}/{slug}.c.o -c {snudown}/src/{slug}.c -Wwrite-strings -D_FORTIFY_SOURCE=0 -DNDEBUG=1"

        for s in slugs:
            cmd = ctmpl.format(odir=OUTPUT_DIR, slug=s, snudown=SNUDOWN)
            file = os.path.join(SNUDOWN, "src", s + ".c")
            assert os.path.isfile(file), "No such file: " + file
            bldr.add_entry(SNUDOWN, cmd, file)

        bldr.write_result(os.path.curdir)

        if not os.path.isdir(OUTPUT_DIR):
            os.mkdir(OUTPUT_DIR)

        for s in slugs:
            translate(s)
    else:
        die("Y U no correct args")


    # if [ "${1}" == "translate" ]; then
    #   echo "[" > compile_commands.json
    #   compile_commands_entry "autolink"
    #   compile_commands_entry "buffer"
    #   compile_commands_entry "stack"
    #   compile_commands_entry "markdown"
    #   echo "]" >> compile_commands.json

    #   mkdir -p $OUTPUT_DIR

    #   translate "autolink"
    #   translate "buffer"
    #   translate "stack"
    #   translate "markdown"

    #   rustc --crate-name=snudownrust --crate-type=staticlib -L $OUTPUT_DIR \
    #       $C2RUST/examples/snudown/snudownrust.rs -o $OUTPUT_DIR/libsnudownrust.a

    # elif [ "$1" == "rustcheck" ]; then

    # TODO: make sure that we built $C2Rust/cross-checks/rust-checks/rustc-plugin

    #   echo "[" > compile_commands.json
    #   compile_commands_entry "autolink"
    #   compile_commands_entry "buffer"
    #   compile_commands_entry "stack"
    #   compile_commands_entry "markdown"
    #   echo "]" >> compile_commands.json

    #   mkdir -p $OUTPUT_DIR

    #   translate_xcheck "autolink"
    #   translate_xcheck "buffer"
    #   translate_xcheck "stack"
    #   translate_xcheck "markdown"

    #   rustc --crate-name=snudownrust --crate-type=staticlib -L $OUTPUT_DIR \
    #       --extern cross_check_derive=$XCHECK_DERIVE \
    #       --extern cross_check_runtime=$XCHECK_RUNTIME \
    #       --cfg "feature=\"cross-check\"" \
    #       $C2RUST/examples/snudown/snudownrust.rs -o $OUTPUT_DIR/libsnudownrustxcheck.a

    # fi
