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
import multiprocessing
from typing import Optional, List, Tuple
from typing.io import TextIO

import mako.template

from common import (
    config as c,
    pb,
    Colors,
    die,
    json_pp_obj,
    get_cmd_or_die,
    on_mac,
    get_system_include_dirs,
    export_ast_from,
    get_rust_toolchain_libpath,
    setup_logging,
)


# Template for the contents of the Cargo.toml file
CARGO_TOML_TEMPLATE = """\
[package]
name = "${crate_name}"
authors = ["C2Rust"]
version = "0.0.0"
publish = false

% if main_module:
[[bin]]
path = "main.rs"
name = "${main_module}"
% else:
[lib]
path = "lib.rs"
crate-type = ["staticlib"]
% endif

% if cross_checks:
[dependencies.cross-check-plugin]
path = "${plugin_path}"

[dependencies.cross-check-derive]
path = "${derive_path}"

[dependencies.cross-check-runtime]
path = "${runtime_path}"
features = ["libc-hash", "fixed-length-array-hash"]

%if use_fakechecks:
[dependencies.libfakechecks-sys]
path = "${libfakechecks_sys_path}"
%endif

% endif
"""

# Template for the crate root lib.rs file
LIB_RS_TEMPLATE = """\
#![feature(libc)]
#![feature(const_ptr_null)]
#![feature(offset_to)]
#![feature(const_ptr_null_mut)]
#![feature(extern_types)]
#![feature(asm)]

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]

% if cross_checks:
#![feature(plugin, custom_attribute)]
#![plugin(cross_check_plugin(${plugin_args}))]
#![cross_check(yes)]

#[macro_use] extern crate cross_check_derive;
#[macro_use] extern crate cross_check_runtime;
% endif

extern crate libc;
% if use_fakechecks:
extern crate libfakechecks_sys;
%endif 

% for (module_name, module_path, line_prefix) in modules:
<% module_name = module_name.replace('-', '_') %>
${line_prefix}#[path = "${module_path}"] pub mod ${module_name};
% endfor

% if main_module:
% if cross_checks:
#[cross_check(none)]
% endif
fn main() { ${main_module}::main() }
% endif
"""


def try_locate_elf_object(cmd: dict) -> Optional[str]:
    # first look for -o in compiler command
    command = None
    if "arguments" in cmd:
        command = " ".join(cmd['arguments'])
    elif "command" in cmd:
        command = cmd['command']
    else:
        die("malformed entry in compile_commands.json:\n" +
            json_pp_obj(cmd))

    if "directory" not in cmd:
        die("malformed entry in compile_commands.json:\n" +
            json_pp_obj(cmd))
    dir = cmd['directory']

    # FIXME: assumes that outfile has .o suffix
    m = re.search(r"\s-o\s+([^\0]+\.o)\s", command)
    if m:
        outfile = m.group(1)
        outpath = os.path.join(dir, outfile)
    else:
        # try replacing suffix of C file with .c
        inpath = os.path.join(dir, cmd['file'])
        outpath = inpath.replace(".c", ".o")

    if os.path.isfile(outpath):
        logging.debug("found output filename: %s", outpath)
        return outpath
    else:
        logging.debug("didn't find output filename for command:\n%s",
                      json_pp_obj(cmd))
        return None


def ensure_code_compiled_with_clang(cc_db: List[dict]) -> None:
    # filter non C code commands first
    c_cc_db = [f for f in cc_db if f['file'].endswith(".c")]
    if not len(c_cc_db):
        msg = "didn't find any commands compiling C files"
        die(msg)

    obj_files = [try_locate_elf_object(c) for c in c_cc_db]
    readelf = get_cmd_or_die("readelf")
    comment_sections = [(f, readelf('-p', '.comment', f))
                        for f in obj_files if f]
    non_clang_files = [(f, c) for (f, c) in comment_sections
                       if "clang" not in c]

    if len(non_clang_files):
        msg = "some ELF objects were not compiled with clang:\n"
        msg += "\n".join([f for (f, c) in comment_sections])
        die(msg)


def write_build_files(dest_dir: str, modules: List[Tuple[str, bool]],
                      main_module: str, cross_checks: bool, 
                      use_fakechecks: bool, cross_check_config: List[str]):
    build_dir = os.path.join(dest_dir, "c2rust-build")
    shutil.rmtree(build_dir, ignore_errors=True)
    os.mkdir(build_dir)

    cargo_toml_path = os.path.join(build_dir, "Cargo.toml")
    with open(cargo_toml_path, "w") as cargo_toml:
        # TODO: allow clients to change the name of the library
        rust_checks_path = os.path.join(c.CROSS_CHECKS_DIR, "rust-checks")
        plugin_path = os.path.join(rust_checks_path, "rustc-plugin")
        derive_path = os.path.join(rust_checks_path, "derive-macros")
        runtime_path = os.path.join(rust_checks_path, "runtime")
        libfakechecks_sys_path = os.path.join(rust_checks_path, 
                                              "backends/libfakechecks-sys")
        tmpl = mako.template.Template(CARGO_TOML_TEMPLATE)
        cargo_toml.write(tmpl.render(
            crate_name="c2rust-build",
            main_module=main_module,
            cross_checks=cross_checks,
            use_fakechecks=use_fakechecks,
            plugin_path=plugin_path,
            derive_path=derive_path,
            runtime_path=runtime_path,
            libfakechecks_sys_path=libfakechecks_sys_path))

    lib_rs_path = os.path.join(build_dir, "main.rs" if main_module else "lib.rs")
    with open(lib_rs_path, "w") as lib_rs:
        template_modules = []
        for (module, module_exists) in modules:
            module_name, _ = os.path.splitext(os.path.basename(module))
            module_relpath = os.path.relpath(module, build_dir)
            line_prefix = '' if module_exists else '#FAILED: '
            template_modules.append((module_name, module_relpath, line_prefix))

        config_files = ('config_file = "{config_file}"'.format(
            config_file=os.path.relpath(ccc, build_dir))
            for ccc in cross_check_config)
        plugin_args = ", ".join(config_files)

        tmpl = mako.template.Template(LIB_RS_TEMPLATE)
        lib_rs.write(tmpl.render(
            main_module=main_module,
            cross_checks=cross_checks,
            use_fakechecks=use_fakechecks,
            plugin_args=plugin_args,
            modules=template_modules))


def check_main_module(main_module: str, cc_db: TextIO):
    """
    check that the main module parameter references a valid
    translation unit in the compile commands database.
    TODO: check that the referenced module actually defines a main method.
    """
    if main_module:
        translation_units = [c['file'] for c in cc_db]
        translation_units = list(map(lambda f: os.path.splitext(f)[0],
                                     translation_units))
        if main_module not in translation_units:
            e = "unknown translation unit: {}\n" \
                .format(main_module)
            e += "must be one of: {}\n".format(", ".join(translation_units))
            die(e, errno.ENOENT)


def transpile_files(cc_db: TextIO,
                    filter: str = None,
                    extra_impo_args: List[str] = [],
                    import_only: bool = False,
                    verbose: bool = False,
                    emit_build_files: bool = True,
                    main_module_for_build_files: str = None,
                    cross_checks: bool = False,
                    use_fakechecks: bool = False,
                    cross_check_config: List[str] = [],
                    reloop_cfgs: bool = True) -> bool:
    """
    run the ast-exporter and ast-importer on all C files
    in a compile commands database.
    """
    ast_expo = get_cmd_or_die(c.AST_EXPO)
    ast_impo = get_cmd_or_die(c.AST_IMPO)
    cc_db_name = cc_db.name
    cc_db = json.load(cc_db)

    check_main_module(main_module_for_build_files, cc_db)

    if filter:  # skip commands not matching file filter
        cc_db = [c for c in cc_db if filter in c['file']]

    if not on_mac():
        ensure_code_compiled_with_clang(cc_db)
    include_dirs = get_system_include_dirs()

    impo_args = ['--translate-entry']
    if emit_build_files:
        impo_args.append('--emit-module')
    if cross_checks:
        impo_args.append('--cross-checks')
        for ccc in cross_check_config:
            impo_args.append('--cross-check-config')
            impo_args.append(ccc)
    if reloop_cfgs:
        impo_args.append('--reloop-cfgs')

    def transpile_single(cmd) -> Tuple[str, int, str, str, str]:

        if import_only:
            cbor_file = os.path.join(cmd['directory'], cmd['file'] + ".cbor")
        else:
            cbor_file = export_ast_from(ast_expo, cc_db_name,
                                        include_dirs, **cmd)
        assert os.path.isfile(cbor_file), "missing: " + cbor_file

        ld_lib_path = get_rust_toolchain_libpath(c.CUSTOM_RUST_NAME)

        # don't overwrite existing ld lib path if any...
        if 'LD_LIBRARY_PATH' in pb.local.env:
            ld_lib_path += ':' + pb.local.env['LD_LIBRARY_PATH']

        # import ast
        with pb.local.env(RUST_BACKTRACE='1',
                          LD_LIBRARY_PATH=ld_lib_path):
            file_basename = os.path.basename(cmd['file'])
            cbor_basename = os.path.basename(cbor_file)
            logging.info(" importing ast from %s", cbor_basename)
            translation_cmd = "RUST_BACKTRACE=1 \\\n"
            translation_cmd += "LD_LIBRARY_PATH=" + ld_lib_path + " \\\n"
            translation_cmd += str(
                ast_impo[cbor_file, impo_args, extra_impo_args])
            logging.debug("translation command:\n %s", translation_cmd)
            try:
                retcode, stdout, stderr = ast_impo[cbor_file, impo_args,
                                                   extra_impo_args].run()

                e = "Expected file suffix `.c.cbor`; actual: " + cbor_basename
                assert cbor_file.endswith(".c.cbor"), e
                rust_file = cbor_file[:-7] + ".rs"
                with open(rust_file, "w") as rust_fh:
                    rust_fh.writelines(stdout)
                    logging.debug("wrote output rust to %s", rust_file)

                return (file_basename, retcode, stdout, stderr,
                        os.path.abspath(rust_file))
            except pb.ProcessExecutionError as pee:
                return (file_basename, pee.retcode, pee.stdout, pee.stderr,
                        None)

    commands = sorted(cc_db, key=lambda cmd: os.path.basename(cmd['file']))
    results = (transpile_single(cmd) for cmd in commands)

    if emit_build_files:
        modules = [(rust_src, retcode == 0) for (_, retcode, _, _, rust_src) in
                   results if rust_src is not None]
        cc_db_dir = os.path.dirname(cc_db_name)
        write_build_files(cc_db_dir,
                          modules,
                          main_module_for_build_files,
                          cross_checks,
                          use_fakechecks,
                          cross_check_config)

    successes, failures = 0, 0
    for (fname, retcode, stdout, stderr, _) in results:
        if not retcode:
            successes += 1
            print(Colors.OKGREEN + " import successful" + Colors.NO_COLOR)
            logging.debug(" import successful")
        else:  # non-zero retcode
            failures += 1
            if verbose:
                print(Colors.FAIL + " import failed" + Colors.NO_COLOR)
                logging.debug(" import failed")
                logging.warning(stderr)
            else:
                print(Colors.FAIL + " import failed (error in log)" +
                      Colors.NO_COLOR)
                logging.debug(" import failed")
                logging.debug(stderr)
    print("translations: " + str(successes + failures))
    print("successes...: " + str(successes))
    print("failures....: " + str(failures))
    return failures == 0


class NegateAction(argparse.Action):
    def __call__(self, parser, ns, values, option):
        setattr(ns, self.dest, option[2:4] != 'no')


def parse_args() -> argparse.Namespace:
    """
    define and parse command line arguments here.
    """
    desc = 'transpile files in compiler_commands.json.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('commands_json', type=argparse.FileType('r'))
    parser.add_argument('-i', '--import-only', default=False,
                        action='store_true', dest='import_only',
                        help='skip ast export step')
    parser.add_argument('-f', '--filter', default="",
                        help='only process files matching filter')
    parser.add_argument('-v', '--verbose', default=False, dest="verbose",
                        help='enable verbose output')
    # parser.add_argument('-j', '--jobs', type=int, dest="jobs",
    #                     default=multiprocessing.cpu_count(),
    #                     help='max number of concurrent jobs')
    parser.add_argument('-a', '--importer-arg', dest="extra_impo_args",
                        default=[], action='append',
                        help='extra arguments for ast-importer')
    parser.add_argument('-e', '--emit-build-files',
                        default=False, action='store_true',
                        help='emit Rust build files, i.e., Cargo.toml '
                             'for a library (or a binary if -m/--main '
                             'is given)')
    parser.add_argument('-m', '--main',
                        default=None, action='store',
                        help='emit Rust build files for a binary using '
                             'the main method in the specified translation '
                             'unit (implies -e/--emit-build-files)')
    parser.add_argument('-x', '--cross-checks',
                        default=False, action='store_true',
                        help='enable cross-checks')
    parser.add_argument('-u', '--use-fakechecks',
                        default=False, action='store_true',
                        help='use log-based cross checking '
                             '(implies -x/--cross-checks)')
    parser.add_argument('-X', '--cross-check-config',
                        default=[], action='append',
                        help='cross-check configuration file(s)')
    parser.add_argument('--reloop-cfgs', '--no-reloop-cfgs', nargs=0,
                        default=True, dest="reloop_cfgs",
                        action=NegateAction,
                        help='enable (disable) relooper; enabled by '
                             'default')
    c.add_args(parser)

    args = parser.parse_args()
    # -m/--main implies -e/--emit-build-files
    args.emit_build_files = True if args.main else args.emit_build_files
    # -u/--use-fakechecks implies -x/--cross-checks
    args.cross_checks = True if args.use_fakechecks else args.cross_checks
    return args


def main():
    setup_logging()
    logging.debug("args: %s", " ".join(sys.argv))

    args = parse_args()
    c.update_args(args)
    transpile_files(args.commands_json,
                    args.filter,
                    args.extra_impo_args,
                    args.import_only,
                    args.verbose,
                    args.emit_build_files,
                    args.main,
                    args.cross_checks,
                    args.use_fakechecks,
                    args.cross_check_config,
                    args.reloop_cfgs)

    logging.info("success")


if __name__ == "__main__":
    main()
