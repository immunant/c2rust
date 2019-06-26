#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from plumbum.cmd import mv, mkdir, rename
from plumbum import local
from typing import Tuple
from common import (
    Colors,
    Config,
    get_cmd_or_die,
    pb,
    setup_logging,
    transpile
)

import argparse
import logging
import multiprocessing
import os
import re
import sys

desc = 'transpile files in compiler_commands.json.'
parser = argparse.ArgumentParser(description="Translates tinycc into the repo/rust/src directory")
parser.add_argument('-f', '--filter',
                    default="",
                    help='Filters translated files')
config = Config()
config.add_args(parser)

C2RUST_DIR = config.ROOT_DIR
TCC_REPO = os.path.join(C2RUST_DIR, "examples/tinycc/repo")
TCC_RS = os.path.join(TCC_REPO, "tcc.rs")
COMPILE_COMMANDS = os.path.join(TCC_REPO, "compile_commands.json")
RUST_ROOT_DIR = os.path.join(TCC_REPO, "rust")
RUST_SRC_DIR = os.path.join(RUST_ROOT_DIR, "src")
MAIN_RS = os.path.join(RUST_SRC_DIR, "main.rs")
MAIN_MODS = """\
#![feature(label_break_value)]
extern crate libc;
pub mod i386_asm;
pub mod libtcc;
pub mod tccasm;
pub mod tccelf;
pub mod tccgen;
pub mod tccpp;
pub mod tccrun;
pub mod x86_64_gen;
pub mod x86_64_link;
"""
Retcode = int
StdErr = str
StdOut = str

def move(from_, to) -> Tuple[Retcode, StdOut, StdErr]:
    mv_args = [from_, to]

    return mv[mv_args].run()

def rename_(*args) -> Tuple[Retcode, StdOut, StdErr]:
    return rename[args].run()

def add_mods(path: str):
    with open(path, "r+") as file:
        text = file.read()
        text = re.sub(r"extern crate libc;", MAIN_MODS, text, count=1)

        file.seek(0)
        file.write(text)
        file.truncate()


if __name__ == "__main__":
    setup_logging()
    args = parser.parse_args()

    # Add option to use the debug version of `c2rust`
    config.update_args(args)

    assert os.path.isfile(COMPILE_COMMANDS), "Could not find {}".format(COMPILE_COMMANDS)

    print(Colors.OKBLUE + "Transpiling..." + Colors.NO_COLOR)
    transpile(COMPILE_COMMANDS, emit_build_files=False,
              reorganize_definitions=False, # TODO
              extra_transpiler_args=["--reduce-type-annotations"])

    # Create the src dir if it doesn't already exist
    mkdir["-p", RUST_SRC_DIR].run()

    # Move and rename TCC.rs to main.rs
    move(TCC_RS, MAIN_RS)

    plumbum_rs_glob = local.path(TCC_REPO) // "*.rs"

    # Move source files to src directory
    retcode, _, _ = move(plumbum_rs_glob, RUST_SRC_DIR)

    assert retcode != 1, "Could not move translated rs files:\n{}".format(stderr)

    # main.rs needs to know about modules so we add them here
    add_mods(MAIN_RS)
    print(Colors.OKGREEN + "Done!" + Colors.NO_COLOR)
