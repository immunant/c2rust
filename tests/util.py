import os
import sys
import yaml
import json
import errno

from typing import Any, List, Iterable, Never, Sequence

CONF_YML: str = "conf.yml"


class Config(object):
    stages: list[str] | None

    def __init__(self, args):
        self.verbose = args.verbose
        self.projects = args.projects  # projects filter
        self.stages = args.stages      # stage filter
        self.ignore_requirements = args.ignore_requirements
        self.project_dirs = find_project_dirs(self)
        self.project_conf = {cf: get_yaml(cf) for cf in get_conf_files(self)}

    def try_get_conf_for(self, conf_file, *keys: str):
        def lookup(yaml, keys: Sequence[str]):
            if not keys:
                return None
            head, *tail = keys
            val = yaml.get(head)
            if val and tail:  # recurse
                return lookup(val, tail)
            else:  # val is None or we reached the last key
                return val

        conf = self.project_conf.get(conf_file)
        if not conf:
            return None
        return lookup(conf, keys)


class Colors(object):
    # Terminal escape codes
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    NO_COLOR = '\033[0m'


def die(emsg: str, status: int=errno.EINVAL) -> Never:
    (red, nc) = (Colors.FAIL, Colors.NO_COLOR)
    print(f"{red}error:{nc} {emsg}", file=sys.stderr)
    exit(status)


def warn(wmsg: str):
    (yellow, nc) = (Colors.WARNING, Colors.NO_COLOR)
    print(f"{yellow}warning:{nc} {wmsg}", file=sys.stderr)


def info(imsg: str):
    (blue, nc) = (Colors.OKBLUE, Colors.NO_COLOR)
    print(f"{blue}info:{nc} {imsg}", file=sys.stdout)


def is_dir_empty(dirp: str):
    return len(os.listdir(dirp)) == 0


def get_script_dir():
    return os.path.dirname(os.path.realpath(__file__))


def get_conf_files(conf) -> Iterable[str]:
    conf_dirs = [get_script_dir()] + conf.project_dirs
    conf_files = map(lambda d: os.path.join(d, CONF_YML), conf_dirs)
    return filter(lambda f: os.path.isfile(f), conf_files)


def find_project_dirs(conf: Config) -> List[str]:
    script_dir = get_script_dir()
    subdirs = sorted(next(os.walk(script_dir))[1])

    # filter out __pycache__ and anything else starting with `_`
    subdirs = list(filter(lambda d: not(d.startswith("_") or d.startswith(".")),
                          subdirs))

    if len(conf.projects) > 0:  # only test named project
        projects = list(filter(lambda d: d in conf.projects, subdirs))
        if len(projects) != len(conf.projects):
            missing_projects = filter(lambda d: d not in projects, conf.projects)
            for project in missing_projects:
                nl = ", ".join(map(lambda p: os.path.basename(p), subdirs))
                y, nc = Colors.WARNING, Colors.NO_COLOR
                msg = f"no such project: {y}{project}{nc}. available projects: {nl}."
                print(msg, file=sys.stderr)
            exit(errno.EINVAL)
        subdirs = projects

    return [os.path.join(script_dir, s) for s in subdirs]


def get_yaml(file: str) -> dict[str, Any]:
    with open(file, 'r') as stream:
        try:
            return yaml.safe_load(stream)
        except yaml.YAMLError as exc:
            die(str(exc))


def check_compile_commands(compile_commands_path: str) -> tuple[bool, str]:
    """
    Return True iff compile_commands_path points to a valid
    compile_commands.json and all referenced source files exist.
    If the commands file exists but source files do not, the second
    tuple value will list the missing files.
    """
    if not os.path.isfile(compile_commands_path):
        return (False, "")

    try:
        with open(compile_commands_path) as fp:
            cc_db = json.load(fp)
    except:
        return (False, "could not open or parse compile commands")

    missing: List[str] = []
    for cmd in cc_db:
        path = os.path.join(cmd["directory"], cmd["file"])
        if not os.path.isfile(path):
            missing += path

    if missing:
        return (False, "\n".join(missing))

    return (True, "PASS")
