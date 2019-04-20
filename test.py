#!/usr/bin/env python3

import repos
import argparse


def get_args():
    parser = argparse.ArgumentParser(description='C2Rust testsuite.')
    parser.add_argument('--verbose', dest='verbose', action='store_true',
                        default=False,
                        help='Enable verbose output')

    return parser.parse_args()

if __name__ == "__main__":
    conf = repos.Config()
    conf.update(get_args())
    repos.run_tests(conf)
