#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from common import Config
from plumbum.cmd import echo, perl
from plumbum import local
from typing import Iterable, Tuple

import argparse
import os

config = Config()

C2RUST_DIR = config.ROOT_DIR
LIBXML2_REPO = os.path.join(C2RUST_DIR, "examples/libxml2/repo")
RUST_ROOT_DIR = os.path.join(LIBXML2_REPO, "rust")
PATCHES = {
    "examples/xmllint.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/runtest.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testC14N.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testHTML.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testReader.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testRelax.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testThreads.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testapi.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testchar.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testlimits.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testrecurse.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testSAX.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testURI.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testAutomata.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testdict.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testModule.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testRegexp.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testSchemas.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testXPath.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
}

# TODO: Better error handling
def replace_all(file_path: str, replacements: Iterable[Tuple[str, str]]) -> None:
    perl_args = ["-000", "-pi"]

    for replace_from, replace_to in replacements:
        perl_args.append("-e")
        perl_args.append("s/{}/{}/g;".format(replace_from, replace_to))

    perl_args.append(file_path)

    retcode, stdout, stderr = perl[perl_args].run()

    assert retcode != 1, "Failed to apply patch {}/replace_all:\n{}".format(file_name, stderr)

if __name__ == "__main__":
    for file_name, patch_config in PATCHES.items():
        file_path = os.path.join(RUST_ROOT_DIR, file_name)

        if "replace_all" in patch_config:
            replace_all(file_path, patch_config["replace_all"])
