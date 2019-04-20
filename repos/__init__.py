
import os
import sys
import subprocess
from . import dependencies
from typing import List  # , Set, Dict, Tuple, Optional


class Config(object):
    # Terminal escape codes
    verbose = False

    def update(self, args):
        self.verbose = args.verbose


class Colors(object):
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

    def __init__(self, conf: Config, dir: str):
        f = next(os.walk(dir))[2]
        self.scripts = set(filter(lambda f: f.endswith(".sh"), f))
        self.dir = dir
        self.name = os.path.basename(dir)
        self.conf = conf

    def run_script(self, stage, script):
        if not self.conf.verbose:
            line = "{name}:{stage}({script})".format(
                name=self.name,
                stage=stage,
                script=script)
            print(line, end="", flush=True)

        script_path = os.path.join(self.dir, script)
        prev_dir = os.getcwd()
        try:
            os.chdir(self.dir)
            if self.conf.verbose:
                subprocess.check_call(args=[script_path])
            else:
                subprocess.check_call(
                    args=[script_path],
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL)

                print("{fill} {color}OK{nocolor}".format(
                    fill=(50 - len(line)) * ".",
                    color=Colors.OKGREEN,
                    nocolor=Colors.NO_COLOR)
                )
        except KeyboardInterrupt as ki:
            if not self.conf.verbose:
                print(": {color}INTERRUPT{nocolor}".format(
                    color=Colors.WARNING,
                    nocolor=Colors.NO_COLOR)
                )
                exit(1)
        except:
            if not self.conf.verbose:
                print(": {color}FAIL{nocolor}".format(
                    color=Colors.FAIL,
                    nocolor=Colors.NO_COLOR)
                )
                exit(1)
        finally:
            os.chdir(prev_dir)

    def __call__(self):
        for (stage, scripts) in Test.STAGES.items():
            for script in scripts:
                if script in self.scripts:
                    self.run_script(stage, script)


def find_test_dirs(conf: Config) -> List[str]:
    script_dir = os.path.dirname(os.path.realpath(__file__))
    subdirs = sorted(next(os.walk(script_dir))[1])

    # filter out __pycache__ and anything else starting with `_`
    subdirs = filter(lambda d: not(d.startswith("_") or d.startswith(".")),
                     subdirs)

    return [os.path.join(script_dir, s) for s in subdirs]


def run_tests(conf):
    dependencies.check(conf)

    tests = (Test(conf, tdir) for tdir in find_test_dirs(conf))

    for t in tests:
        t()
