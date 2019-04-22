#!/usr/bin/env python3

import tests
import argparse


def get_args():
    parser = argparse.ArgumentParser(description='C2Rust testsuite.')
    parser.add_argument('--verbose', dest='verbose', action='store_true',
                        default=False,
                        help='Enable verbose output')
    parser.add_argument('--only', dest='only', action='store',
                        type=str, default=None,
                        help='Run named test only')

    return parser.parse_args()


if __name__ == "__main__":
    conf = tests.Config()
    conf.update(get_args())
    tests.run_tests(conf)
