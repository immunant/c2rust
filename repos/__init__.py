
import os
import sys
import subprocess

# import repos.lua
# import repos.zstd

# TEST_FUNCTION: str = "c2rust_test"


def configure():
    print("configuring")


def dependencies():
    # TODO: check dependencies
    pass


class Colors:
    # Terminal escape codes
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    NO_COLOR = '\033[0m'

class Test(object):

    STAGES: dict = {
        "configure": ["configure.sh"],
        "make": ["make.sh", "cmake.sh"],
        "transpile": ["transpile.sh"],
        "check": ["check.sh", "test.sh"]
    }

    def __init__(self, dir: str):
        f = next(os.walk(dir))[2]
        self.scripts = set(filter(lambda f: f.endswith(".sh"), f))
        self.dir = dir
        self.name = os.path.basename(dir)

    def __call__(self):
        prev_dir = os.getcwd()
        try:
            os.chdir(self.dir)
            for (stage, scripts) in Test.STAGES.items():
                for script in scripts:
                    if script in self.scripts:
                        print("{}:{}({})...".format(name, stage, script))
                        conf_path = os.path.join(self.dir, script)
                        subprocess.check_call([conf_path])
                        break
        finally:
            os.chdir(prev_dir)


def find_tests():
    script_dir = os.path.dirname(os.path.realpath(__file__))
    subdirs = next(os.walk(script_dir))[1]

    # filter out __pycache__ and anything else starting with `_`
    subdirs = filter(lambda d: not(d.startswith("_") or d.startswith(".")),
                     subdirs)

    return [Test(os.path.join(script_dir, s)) for s in subdirs]


def main():
    # for (name, mod) in sys.modules.items():
    #     if TEST_FUNCTION in dir(mod):
    #         test_fn = getattr(mod, TEST_FUNCTION)
    #         test_fn()
    dependencies()

    tests = find_tests()
    for t in tests:
        t()

    



