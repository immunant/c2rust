#!/usr/bin/env python3

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
    parser.add_argument('--project', dest='project', action='store',
                        type=str, default=None,
                        help='Only test specified project')
    parser.add_argument('--stage', dest='stage', action='store',
                        type=str, default=None,
                        help='Only test specified stage')
    parser.add_argument('--print-requirements', metavar='PLATFORM',
                        dest='requirements', choices=['ubuntu'],
                        action='store', type=str, default=None,
                        help='Print requirements for platform and exit')
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
    elif not conf.project_dirs and args.project:
        util.die(f"no such project: {args.project}")
    else:
        templates.autogen(conf)
        tests.run_tests(conf)
