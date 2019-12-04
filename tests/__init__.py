
import os
import sys
import subprocess
from typing import List  # , Set, Dict, Tuple, Optional

from tests.util import *
from tests.requirements import *


class Test(object):

    STAGES: dict = {
        "autogen": ["autogen.sh"],
        "configure": ["configure.sh"],
        "make": ["make.sh", "cmake.sh"],
        "transpile": ["transpile.gen.sh", "transpile.sh"],
        "cargo": ["cargo.gen.sh", "cargo.sh"],
        "check": ["check.sh", "test.sh"]
    }

    def __init__(self, directory: str):
        ff = next(os.walk(directory))[2]
        self.scripts = set(filter(lambda f: f.endswith(".sh"), ff))
        self.dir = directory
        self.conf_file = os.path.join(directory, CONF_YML)
        self.name = os.path.basename(directory)

    def run_script(self, stage, script, verbose=False, xfail=False) -> bool:
        """
        Returns true iff subsequent test stages should run
        """

        def print_log_tail_on_fail(script_path):
            logfile = f"{script_path}.log"
            if os.path.isfile(logfile):
                proc = subprocess.Popen(['tail', '-n', '20', logfile], stdout=subprocess.PIPE)
                for line in proc.stdout:
                    print(line.decode().rstrip())
            else:
                print("{color}Missing log file: {logf}{nocolor}".format(
                    color=Colors.WARNING,
                    logf=logfile,
                    nocolor=Colors.NO_COLOR)
                )

        prev_dir = os.getcwd()
        script_path = os.path.join(self.dir, script)

        if not os.path.isfile(script_path):
            print("{color}Missing script: {script}{nocolor}".format(
                    color=Colors.FAIL,
                    script=script_path,
                    nocolor=Colors.NO_COLOR)
                )
            return False

        if not os.access(script_path, os.X_OK):
            print("{color}Script is not executable: {script}{nocolor}".format(
                    color=Colors.FAIL,
                    script=script_path,
                    nocolor=Colors.NO_COLOR)
                )
            return False

        if not verbose:
            relpath = os.path.relpath(script_path, prev_dir)
            line = "{blue}{name}{nc}: {stage}({script})".format(
                blue=Colors.OKBLUE,
                name=self.name,
                nc=Colors.NO_COLOR,
                stage=stage,
                script=relpath)
            print(line, end="", flush=True)

        # if we already have `compile_commands.json`, skip the build stages
        if stage in ["autogen", "configure", "make"]:
            compile_commands = os.path.join(self.dir, "compile_commands.json")
            compile_commands_present = os.path.isfile(compile_commands)
            if compile_commands_present:
                fill = (75 - len(line)) * "."
                color = Colors.OKBLUE
                msg = "OK_CACHED"
                print(f"{fill} {color}{msg}{Colors.NO_COLOR}")
                return True

        success = False

        # noinspection PyBroadException
        try:
            os.chdir(self.dir)
            if verbose:
                subprocess.check_call(args=[script_path])
            else:
                subprocess.check_call(
                    args=[script_path],
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL)

                fill = (75 - len(line)) * "."
                color = Colors.WARNING if xfail else Colors.OKGREEN
                msg = "OK_XFAIL" if xfail else "OK"
                print(f"{fill} {color}{msg}{Colors.NO_COLOR}")
            success = True
        except KeyboardInterrupt:
            if not verbose:
                print(": {color}INTERRUPT{nocolor}".format(
                    color=Colors.WARNING,
                    nocolor=Colors.NO_COLOR)
                )
            exit(1)
        except Exception:  # noqa
            if not verbose:
                outcome = "XFAIL" if xfail else "FAIL"
                print("{fill} {color}{outcome}{nocolor}".format(
                    fill=(75 - len(line)) * ".",
                    color=Colors.OKBLUE if xfail else Colors.FAIL,
                    outcome=outcome,
                    nocolor=Colors.NO_COLOR)
                )
                print_log_tail_on_fail(script_path)
        finally:
            os.chdir(prev_dir)
            return success

    def ensure_submodule_checkout(self):
        # make sure the `repo` directory exists and is not empty
        repo_dir = os.path.join(self.dir, "repo")
        if not os.path.isdir(repo_dir):
            die(f"missing directory: {repo_dir}")
        elif is_dir_empty(repo_dir):
            repo_dir = os.path.relpath(repo_dir, os.path.curdir)
            msg = f"submodule not checked out: {repo_dir}\n"
            msg += f"(try running `git submodule update --depth 50 --init {repo_dir}`)"
            die(msg)

    def is_stage_xfail(self, stage, script, conf: Config) -> bool:
        def has_xfail_file() -> bool:
            script_path = os.path.join(self.dir, script)
            if os.path.isfile(f"{script_path}.xfail"):
                return True
            script_path_noext = script_path.replace(".sh", ".xfail")
            if os.path.isfile(script_path_noext):
                return True
            gen_script_path_noext = script_path.replace(".gen.sh", "")
            return os.path.isfile(f"{gen_script_path_noext}.xfail")

        if has_xfail_file():
            return True

        xfail = conf.try_get_conf_for(self.conf_file, stage, "xfail")
        if not xfail:
            return False
        if not isinstance(xfail, bool):
            die(f"expected boolean xfail value; found {xfail}")
        return xfail

    def __call__(self, conf: Config):
        """Returns true if test was successful or expected to fail, false on unexpected
        failure
        """

        self.ensure_submodule_checkout()

        stages = Test.STAGES.keys()
        if conf.stages:
            # Check that all stages are valid
            for stage in conf.stages:
                if stage not in Test.STAGES:
                    # invalid stage requested
                    requested_stages = ", ".join(conf.stages)
                    stages = ", ".join(Test.STAGES.keys())
                    y, nc = Colors.WARNING, Colors.NO_COLOR
                    die(f"invalid stages: {y}{requested_stages}{nc}. valid stages: {stages}")

            stages = conf.stages

        for stage in stages:
            for script in Test.STAGES[stage]:
                if script in self.scripts:
                    xfail = self.is_stage_xfail(stage, script, conf)
                    cont = self.run_script(stage, script, conf.verbose, xfail)
                    if not cont:
                        return xfail
                    break  # found script for stage; skip alternatives
        return True


def run_tests(conf):
    if not conf.ignore_requirements:
        requirements.check(conf)

    tests = [Test(td) for td in conf.project_dirs]

    failure = False
    for tt in tests:
        failure |= not tt(conf)

    if failure:
        exit(1)
