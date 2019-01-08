#!/usr/bin/env python3

from common import (
    config as c,
    get_cmd_or_die,
    invoke,
    pb,
    setup_logging,
)


CRATES_TO_PACKAGE = [
    c.AST_BUILDER_CRATE_DIR,
    c.AST_EXPORTER_CRATE_DIR,
    c.BITFIELDS_CRATE_DIR,
    c.TRANSPILE_CRATE_DIR,
    c.REFACTOR_CRATE_DIR,
    c.C2RUST_DIR,
]


def package(crate_dir):
    cargo = get_cmd_or_die("cargo")
    with pb.local.cwd(crate_dir):
        invoke(cargo['package', '--no-verify'])


def main():
    setup_logging()
    for crate in CRATES_TO_PACKAGE:
        package(crate)


if __name__ == "__main__":
    main()
