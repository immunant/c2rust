#!/usr/bin/env -S uv run

import argparse

import tests
import tests.requirements as requirements
import tests.templates as templates
import tests.util as util


def get_args():
    parser = argparse.ArgumentParser(description='C2Rust testsuite.')
    parser.add_argument('--verbose', dest='verbose', action='store_true',
                        default=False,
                        help='Enable verbose output')
    parser.add_argument('--stages', dest='stages', action='store',
                        nargs='*', type=str, default=None, choices=tests.Test.STAGES,
                        help='Only test specified stage(s)')
    parser.add_argument('--print-requirements', metavar='PLATFORM',
                        dest='requirements', choices=['ubuntu'],
                        action='store', type=str, default=None,
                        help='Print requirements for platform and exit')
    parser.add_argument('--ignore-requirements',
                        action='store_true',
                        help='Ignore test requirements')
    parser.add_argument('projects', metavar='project', type=str, nargs='*',
                        help='Project to test (defaults to all projects if none specified)')
    return parser.parse_args()


def print_requirements(args):
    packages = sorted(requirements.collect(conf, args.requirements))
    packages = " \\\n".join(packages)
    print(packages)


if __name__ == "__main__":
    args = get_args()
    conf = tests.Config(args)
    if args.requirements:
        print_requirements(args)
    elif not conf.project_dirs and len(args.projects) > 0:
        util.die(f"no such project: {args.project}")
    else:
        templates.autogen(conf)
        tests.run_tests(conf)
