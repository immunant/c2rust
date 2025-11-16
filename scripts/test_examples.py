#!/usr/bin/env -S uv run

import argparse
import errno
import logging
import os
import multiprocessing
import re
import sys

from typing import Dict, List, Optional

from common import (
    config as c,
    pb,
    Colors,
    die,
    get_cmd_or_die,
    invoke,
    regex,
    setup_logging,
    transpile,
    on_mac
)

cargo = get_cmd_or_die('cargo')
git = get_cmd_or_die('git')
intercept_build = get_cmd_or_die('intercept_build')
make = get_cmd_or_die('make')
python = get_cmd_or_die('python')
rustc = get_cmd_or_die('rustc')

NUM_JOBS = multiprocessing.cpu_count()

EXAMPLES = [
    'genann',
    'grabc',
    'libxml2',
    'lil',
    'snudown',
    'tmux',
    'urlparser',
    'xzoom'
]


def build_path(path: str, new: str, is_dir: bool) -> str:
    err_msg = "`{}` does not exist in {}".format(new, path)

    new_path = os.path.join(path, new)
    if is_dir:
        assert os.path.isdir(new_path), err_msg
    else:
        assert os.path.isfile(new_path), err_msg
    return new_path


def print_blue(msg: str) -> None:
    print(Colors.OKBLUE + msg + Colors.NO_COLOR)


class Test:
    def __init__(self, args: argparse.Namespace):
        self.args = args
        self.project_name = ''
        self.transpiler_args: List[str] = []
        self.ib_cmd: List[str] = []
        self.example_dir = ''
        self.repo_dir = ''
        # Source directory where `Crate` files will live,
        # e.g. `rust`
        self.rust_src = ''
        self.cc_db = ''

    def init_submodule(self) -> None:
        if self.args.regex_examples.fullmatch(self.project_name):
            print_blue("Initializing {}...".format(self.project_name))
            with pb.local.cwd(self.example_dir):
                invoke(git, ['submodule', 'update', '--init', 'repo'])

    def deinit_submodule(self) -> None:
        if self.args.regex_examples.fullmatch(self.project_name) and self.args.deinit:
            print_blue("Deinitializing {}...".format(self.project_name))
            with pb.local.cwd(self.example_dir):
                invoke(git, ['submodule', 'deinit', 'repo', '-f'])

    # Should be used on projects that utilize GNU Build Systems
    def autotools(self, configure_args: List[str] = []) -> None:
        with pb.local.cwd(self.repo_dir):
            invoke(pb.local['./autogen.sh'])
            with pb.local.env(CFLAGS="-g -O0"):
                invoke(pb.local['./configure'], configure_args)

    # `gen_cc_db` generates the `compile_commands.json` for a project
    def gen_cc_db(self) -> None:
        with pb.local.cwd(self.repo_dir):
            invoke(make, ['clean'])
            invoke(intercept_build, *self.ib_cmd)
            self.cc_db = build_path(self.repo_dir, 'compile_commands.json',
                                    is_dir=False)

    # `transpile` in most cases runs the transpile function from `common.py`,
    # which in turn just calls `c2rust transpile *args`
    def transpile(self) -> None:
        with pb.local.cwd(self.repo_dir):
            transpile(self.cc_db,
                      emit_build_files=False,
                      extra_transpiler_args=self.transpiler_args)

    # `build` is the main builder function, this is where either the `Crate`
    # will be built or rustc will be called directly
    def build(self) -> None:
        with pb.local.cwd(self.rust_src):
            invoke(cargo, ['build', '-j{}'.format(NUM_JOBS)])

    def test(self) -> None:
        pass

    def build_and_test(self) -> None:
        self.gen_cc_db()
        self.transpile()
        self.build()
        self.test()


class Genann(Test):
    def __init__(self, args: argparse.Namespace) -> None:
        self.args = args
        self.project_name = 'genann'
        self.example_dir = build_path(
            c.EXAMPLES_DIR, self.project_name, is_dir=True)
        self.repo_dir = build_path(self.example_dir, 'repo', is_dir=True)
        self.rust_src = os.path.join(self.example_dir, 'rust')
        self.transpiler_args = ['--emit-build-files', '--overwrite-existing',
                                '--output-dir', self.rust_src]
        self.ib_cmd = ['make']
        self.init_submodule()

    def __del__(self) -> None:
        self.deinit_submodule()

    def test(self) -> None:
        rm = get_cmd_or_die('rm')
        ln = get_cmd_or_die('ln')
        for N in (1, 4):
            test = 'example{}'.format(N)
            with pb.local.cwd(self.repo_dir):
                invoke(rm, ['-rf', self.rust_src])

            self._transpile_example(test)
            with pb.local.cwd(self.rust_src):
                # Create a link to the example data files
                invoke(ln, ['-sf', build_path(self.repo_dir, 'example', True)])
                invoke(cargo, ['run'])

    # Helper function that transpiles whatever test is
    # passed in as `main`
    def _transpile_example(self, main: str) -> None:
        transpile(self.cc_db,
                  emit_build_files=False,
                  extra_transpiler_args=['--emit-build-files', '--binary', main,
                                         '--overwrite-existing',
                                         '--output-dir', self.rust_src])


class Grabc(Test):
    def __init__(self, args: argparse.Namespace) -> None:
        self.args = args
        self.project_name = 'grabc'
        self.transpiler_args = ['--overwrite-existing']
        self.ib_cmd = ['make']
        self.example_dir = build_path(
            c.EXAMPLES_DIR, self.project_name, is_dir=True)
        self.repo_dir = build_path(self.example_dir, 'repo', is_dir=True)
        self.build_flags = ['grabc.rs', '-L/usr/x11R6/lib', '-lX11']
        self.init_submodule()

    def __del__(self) -> None:
        self.deinit_submodule()

    def build(self) -> None:
        with pb.local.cwd(self.repo_dir):
            invoke(rustc, *self.build_flags)


class Libxml2(Test):
    def __init__(self, args: argparse.Namespace) -> None:
        self.args = args
        self.project_name = 'libxml2'
        self.transpiler_args = []
        self.ib_cmd = ['make', 'check', '-j{}'.format(NUM_JOBS)]
        self.example_dir = build_path(
            c.EXAMPLES_DIR, self.project_name, is_dir=True)
        self.repo_dir = build_path(self.example_dir, 'repo', is_dir=True)
        self.build_flags: List[str] = []
        self.init_submodule()
        self.rust_src = os.path.join(self.repo_dir, 'rust')

    def __del__(self) -> None:
        self.deinit_submodule()

    def gen_cc_db(self) -> None:
        # Without --disable-static, libtool builds two copies of many source
        # files. We can't handle that, so we disable that behavior here.
        self.autotools(['--disable-static'])
        with pb.local.cwd(self.repo_dir):
            invoke(make, ['clean'])
            invoke(intercept_build, *self.ib_cmd)

    def transpile(self) -> None:
        with pb.local.cwd(self.example_dir):
            invoke(pb.local['./translate.py'])
            invoke(pb.local['./patch_translated_code.py'])

    # Iterates through the list of tests, and then runs each one
    def test(self) -> None:
        # testname -> input_file
        tests: Dict[str, List[str]] = {
            "xmllint": ['test/bigname.xml'],
            "runtest": [],
            "testapi": [],
            "testSAX": [],
            "testURI": ['test/bigname.xml'],
            "testdict": [],
            "testHTML": ['test/HTML/html5_enc.html'],
            "testC14N": ['--', '--with-comments', 'test/c14n/with-comments/example-7.xml'],
            "testchar": [],
            "testRelax": ['test/bigname.xml'],
            "testXPath": ['test/bigname.xml'],
            "testModule": [],
            "testlimits": [],
            # "testReader", Not working at the moment
            "testRegexp": ['test/regexp'],
            "testrecurse": [],
            "testSchemas": ['test/schemas/all_0.xsd'],
            "testThreads": [],
            "testAutomata": ['test/automata/po'],
        }

        for test, input_file in tests.items():
            with pb.local.cwd(self.rust_src):
                example_args = ['run', '--example', test]
                example_args.extend(input_file)
                invoke(cargo, *example_args)


class Lil(Test):
    def __init__(self, args: argparse.Namespace) -> None:
        self.args = args
        self.project_name = 'lil'
        self.example_dir = build_path(
            c.EXAMPLES_DIR, self.project_name, is_dir=True)
        self.repo_dir = build_path(self.example_dir, 'repo', is_dir=True)
        self.rust_src = os.path.join(self.repo_dir, 'rust')
        self.transpiler_args = ['--emit-build-files', '-b', 'main',
                                '--overwrite-existing',
                                '--output-dir', self.rust_src]
        self.ib_cmd = ['make']
        self.build_flags: List[str] = []
        self.init_submodule()

    def __del__(self) -> None:
        self.deinit_submodule()


class Snudown(Test):
    def __init__(self, args: argparse.Namespace) -> None:
        self.args = args
        self.project_name = 'snudown'
        self.transpiler_args = ['--overwrite-existing']
        self.ib_cmd = []
        self.example_dir = build_path(
            c.EXAMPLES_DIR, self.project_name, is_dir=True)
        self.repo_dir = build_path(self.example_dir, 'repo', is_dir=True)
        self.build_flags = ['setup.py', 'build', '--translate']
        self.init_submodule()

    def __del__(self) -> None:
        self.deinit_submodule()

    def gen_cc_db(self) -> None:
        pass

    def transpile(self) -> None:
        pass

    def build(self) -> None:
        with pb.local.cwd(self.repo_dir):
            invoke(python, *self.build_flags)

    def test(self) -> None:
        with pb.local.cwd(self.repo_dir):
            invoke(python, ['setup.py', 'test'])

class TinyCC(Test):
    def __init__(self, args: argparse.Namespace) -> None:
        self.args = args
        self.project_name = 'tinycc'
        self.transpiler_args = []
        self.ib_cmd = ['make', '-j{}'.format(NUM_JOBS)]
        self.example_dir = build_path(
            c.EXAMPLES_DIR, self.project_name, is_dir=True)
        self.repo_dir = build_path(self.example_dir, 'repo', is_dir=True)
        self.build_flags: List[str] = []
        self.init_submodule()
        self.rust_src = os.path.join(self.repo_dir, 'rust')

    def __del__(self) -> None:
        self.deinit_submodule()

    def autotools(self, configure_args: List[str] = []) -> None:
        os.chdir(self.repo_dir)
        invoke(pb.local['./configure'])

    def gen_cc_db(self) -> None:
        self.autotools()
        with pb.local.cwd(self.repo_dir):
            invoke(make, ['clean'])
            invoke(intercept_build, *self.ib_cmd)

    def transpile(self) -> None:
        with pb.local.cwd(self.example_dir):
            invoke(pb.local['./translate.py'])

    def test(self) -> None:
        with pb.local.cwd(self.repo_dir):
            invoke(make, ['rust-test'])

class Tmux(Test):
    def __init__(self, args: argparse.Namespace) -> None:
        self.args = args
        self.project_name = 'tmux'
        self.transpiler_args = []
        self.ib_cmd = ['make', 'check', '-j{}'.format(NUM_JOBS)]
        self.example_dir = build_path(
            c.EXAMPLES_DIR, self.project_name, is_dir=True)
        self.repo_dir = build_path(self.example_dir, 'repo', is_dir=True)
        self.build_flags: List[str] = []
        self.init_submodule()
        self.rust_src = os.path.join(self.repo_dir, 'rust')

    def __del__(self) -> None:
        self.deinit_submodule()

    def gen_cc_db(self) -> None:
        self.autotools()
        with pb.local.cwd(self.repo_dir):
            invoke(make, ['clean'])
            invoke(intercept_build, *self.ib_cmd)

    def transpile(self) -> None:
        with pb.local.cwd(self.example_dir):
            invoke(pb.local['./translate.py'])


class Urlparser(Test):
    def __init__(self, args: argparse.Namespace) -> None:
        self.args = args
        self.project_name = 'urlparser'
        self.transpiler_args = ['--overwrite-existing']
        self.ib_cmd = ['make']
        self.example_dir = build_path(
            c.EXAMPLES_DIR, self.project_name, is_dir=True)
        self.repo_dir = build_path(self.example_dir, 'repo', is_dir=True)
        self.build_flags = ['test.rs']
        self.init_submodule()

    def __del__(self) -> None:
        self.deinit_submodule()

    def build(self) -> None:
        with pb.local.cwd(self.repo_dir):
            invoke(rustc, *self.build_flags)


class Xzoom(Test):
    def __init__(self, args: argparse.Namespace) -> None:
        self.args = args
        self.project_name = 'xzoom'
        self.transpiler_args = ['--overwrite-existing']
        self.ib_cmd = ['sh', '-c',
                       'clang xzoom.c -L/usr/X11R6/lib -lX11 -DTIMER']
        self.example_dir = build_path(
            c.EXAMPLES_DIR, self.project_name, is_dir=True)
        self.repo_dir = build_path(self.example_dir, 'repo', is_dir=True)
        self.build_flags = ['xzoom.rs', '-L/usr/x11R6/lib', '-lX11']
        self.init_submodule()

    def __del__(self) -> None:
        self.deinit_submodule()

    def gen_cc_db(self) -> None:
        with pb.local.cwd(self.repo_dir):
            invoke(intercept_build, *self.ib_cmd)
            self.cc_db = build_path(self.repo_dir, 'compile_commands.json',
                                    is_dir=False)

    def build(self) -> None:
        with pb.local.cwd(self.repo_dir):
            invoke(rustc, *self.build_flags)


def _is_excluded(name: str) -> bool:
    """
    The examples that use x11 and the `f128` crate need to be excluded on macOS.
    This function can be extended to exclude examples
    on the linux platform as well.
    """
    mac_exclusion_set = {
        "grabc",
        "libxml2",
        "tinycc",
        "xzoom",
    }

    return name in mac_exclusion_set and on_mac()


def _parser_args() -> argparse.Namespace:
    desc = 'Build and test examples.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument(
        '--only-examples', dest='regex_examples', type=regex, default='.*',
        help="Regular Expression to filter which example to build and run"
    )
    parser.add_argument('--deinit', default=False,
                        action='store_true', dest='deinit',
                        help='Deinitialize the submodules, this will remove\
                        all unstaged changes')
    c.add_args(parser)
    args = parser.parse_args()
    c.update_args(args)
    return args


def run(args: argparse.Namespace) -> None:
    examples = [
        Genann(args),
        Grabc(args),
        Libxml2(args),
        Lil(args),
        Snudown(args),
        TinyCC(args),
        Tmux(args),
        Urlparser(args),
        Xzoom(args),
    ]
    for example in examples:
        if args.regex_examples.fullmatch(example.project_name) and\
        not _is_excluded(example.project_name):
            example.build_and_test()

    print(Colors.OKGREEN + "Done building and testing the examples." +
          Colors.NO_COLOR)


def main() -> None:
    setup_logging()
    args = _parser_args()
    run(args)


if __name__ == "__main__":
    main()
