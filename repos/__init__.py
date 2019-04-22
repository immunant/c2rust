
import os
import sys
import subprocess
from typing import List  # , Set, Dict, Tuple, Optional

from repos.util import *
from repos.requirements import *

REQUIREMENTS_YML: str = "requirements.yml"


class Config(object):
    # Terminal escape codes
    verbose = False
    only = None

    def update(self, args):
        self.verbose = args.verbose
        self.only = args.only


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

    def run_script(self, stage, script, xfail=False) -> bool:
        """
        Returns true iff subsequent tests should run
        """
        prev_dir = os.getcwd()
        script_path = os.path.join(self.dir, script)
        if not self.conf.verbose:
            relpath = os.path.relpath(script_path, prev_dir)
            line = "{blue}{name}{nc}: {stage}({script})".format(
                blue=Colors.OKBLUE,
                name=self.name,
                nc=Colors.NO_COLOR,
                stage=stage,
                script=relpath)
            print(line, end="", flush=True)

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
                    fill=(75 - len(line)) * ".",
                    color=Colors.OKGREEN,
                    nocolor=Colors.NO_COLOR)
                )
                return True
        except KeyboardInterrupt as ki:
            if not self.conf.verbose:
                print(": {color}INTERRUPT{nocolor}".format(
                    color=Colors.WARNING,
                    nocolor=Colors.NO_COLOR)
                )
                exit(1)
        except:
            if not self.conf.verbose:
                outcome = "XFAIL" if xfail else "FAIL"
                print(": {color}{outcome}{nocolor}".format(
                    color=Colors.WARNING if xfail else Colors.FAIL,
                    outcome=outcome,
                    nocolor=Colors.NO_COLOR)
                )
            if not xfail:
                exit(1)
            else:
                return False
        finally:
            os.chdir(prev_dir)

    def is_xfail(self, script):
        xfail_path = os.path.join(self.dir, f"{script}.xfail")
        return os.path.isfile(xfail_path)

    def __call__(self):
        for (stage, scripts) in Test.STAGES.items():
            for script in scripts:
                if script in self.scripts:
                    xfail = self.is_xfail(script)
                    cont = self.run_script(stage, script, xfail)
                    if not cont:
                        return  # XFAIL


def get_script_dir():
    return os.path.dirname(os.path.realpath(__file__))


def find_test_dirs(conf: Config) -> List[str]:
    script_dir = get_script_dir()
    subdirs = sorted(next(os.walk(script_dir))[1])

    # filter out __pycache__ and anything else starting with `_`
    subdirs = filter(lambda d: not(d.startswith("_") or d.startswith(".")),
                     subdirs)

    return [os.path.join(script_dir, s) for s in subdirs]


def find_requirements(conf: Config) -> List[str]:
    script_dir = get_script_dir()
    subdirs = sorted(next(os.walk(script_dir))[1])

    if conf.only:
        subdirs = filter(lambda s: s == conf.only, subdirs)

    reqs = os.path.join(script_dir, REQUIREMENTS_YML)
    reqs = [reqs] if os.path.exists(reqs) else []

    subreqs = map(lambda dir: os.path.join(script_dir, dir, REQUIREMENTS_YML),
                  subdirs)
    reqs += filter(lambda f: os.path.exists(f), subreqs)
    return reqs


def run_tests(conf):
    tests = (Test(conf, tdir) for tdir in find_test_dirs(conf))

    if conf.only:
        only_test = list(filter(lambda t: t.name == conf.only, tests))
        if not only_test:
            nl = ", ".join(map(lambda p: os.path.basename(p), find_test_dirs(conf)))
            y, nc = Colors.WARNING, Colors.NO_COLOR
            msg = f"no such test: {y}{conf.only}{nc}. valid test names: {nl}."
            die(msg)
        else:
            tests = only_test

    for r in find_requirements(conf):
        requirements.check(conf, r)

    for t in tests:
        t()
