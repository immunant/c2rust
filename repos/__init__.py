
import sys

import repos.lua
import repos.zstd

TEST_FUNCTION: str = "c2rust_test"

def configure():
    print("configuring")


def main():
    for (name, mod) in sys.modules.items():
        if TEST_FUNCTION in dir(mod):
            test_fn = getattr(mod, TEST_FUNCTION)
            test_fn()