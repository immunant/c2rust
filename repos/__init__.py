
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
                        line = "{name}:{stage}({script})".format(
                                name=self.name,
                                stage=stage,
                                script=script)
                        print(line, end="", flush=True)
                        conf_path = os.path.join(self.dir, script)
                        subprocess.check_call(
                            args=[conf_path],
                            stdout=subprocess.DEVNULL,
                            stderr=subprocess.DEVNULL)
                        print("{fill} {color}OK{nocolor}".format(
                            fill=(50-len(line)) * ".",
                            color=Colors.OKGREEN,
                            nocolor=Colors.NO_COLOR)
                        )
                        break
        except KeyboardInterrupt:
            print(": {color}INTERRUPT{nocolor}".format(
                color=Colors.WARNING,
                nocolor=Colors.NO_COLOR)
            )
        except:
            print(": {color}FAIL{nocolor}".format(
                color=Colors.FAIL,
                nocolor=Colors.NO_COLOR)
            )
        finally:
            os.chdir(prev_dir)


def find_tests():
    script_dir = os.path.dirname(os.path.realpath(__file__))
    subdirs = sorted(next(os.walk(script_dir))[1])

    # filter out __pycache__ and anything else starting with `_`
    subdirs = filter(lambda d: not(d.startswith("_") or d.startswith(".")),
                     subdirs)

    return [Test(os.path.join(script_dir, s)) for s in subdirs]


def main():
    # TODO: support command line args
    # --support verbose mode showing output

    # for (name, mod) in sys.modules.items():
    #     if TEST_FUNCTION in dir(mod):
    #         test_fn = getattr(mod, TEST_FUNCTION)
    #         test_fn()
    dependencies()

    tests = find_tests()
    for t in tests:
        t()

    



