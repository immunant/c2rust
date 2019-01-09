#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# This script builds a C2Rust translated version of snudown,
# either with or without cross-checks

from shutil import rmtree
from common import *
from collections import namedtuple


CompileCommand = namedtuple('CompileCommand', ['directory', 'arguments', 'file'])


class CompileCommandsBuilder(object):
    entries: List[CompileCommand] = []

    def add_entry(self, dir: str, args: [], file: str) -> None:
        entry = CompileCommand(directory=dir, arguments=args, file=file)
        self.entries.append(entry._asdict())

    def write_result(self, outdir: str) -> None:
        assert os.path.isdir(outdir), "No such dir: " + outdir
        outpath = os.path.join(outdir, config.CC_DB_JSON)
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

    # make sure the snudown submodule is checked out and up to date
    # update_or_init_submodule(snudown)

    generate_html_entries_header(snudown)

    bldr = CompileCommandsBuilder()

    slugs = ["autolink", "buffer", "stack", "markdown"]
    ctmpl = "cc -o {snudown}/src/{slug}.c.o -c {snudown}/src/{slug}.c -Isrc -Ihtml -Wwrite-strings -D_FORTIFY_SOURCE=0 -DNDEBUG=1"

    for s in slugs:
        args = ctmpl.format(slug=s, snudown=snudown)
        arguments = [c for c in args.split()]
        file = os.path.join(snudown, "src", s + ".c")
        assert os.path.isfile(file), "No such file: " + file
        bldr.add_entry(snudown, arguments, file)

    cmds_json_path = bldr.write_result(os.path.curdir)
    config_path = os.path.join(snudown, "xchecks/snudown_rust.yaml")

    transpile(cmds_json_path, emit_build_files=True,
              cross_checks=xcheck, cross_check_config=config_path)

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

