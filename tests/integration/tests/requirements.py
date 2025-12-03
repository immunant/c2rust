import subprocess
from typing import Dict, List, Set

import tests.hostenv as hostenv
from tests.util import *


def check_apt_package(yaml: List[str]):
    assert isinstance(yaml, List), "expected list of apt packages"
    errors = []
    for p in yaml:
        args = ["dpkg", "-l", p]

        try:
            output: bytes = subprocess.check_output(args)
        except subprocess.CalledProcessError:
            errors.append(f"package not installed: {p}")
            continue

        output: str = output.decode()
        last: str = output.splitlines()[-1]
        expected: str = f"ii  {p}"
        if not last.startswith(expected):
            errors.append(f"package not (properly) installed: {p} (dpkg output: {output}) ")
    
    if errors:
        errors = "\n".join(errors)
        die(errors)


def check_apt(yaml: Dict):
    packages = yaml.pop("packages")
    if packages:
        check_apt_package(packages)
    if yaml:
        warn(f"unhandled requirements: {yaml}")


def check_programs_in_path(yaml: Dict):
    assert isinstance(yaml, List), "expected list of apt packages"

    for p in yaml:
        args = ["which", p]
        try:
            subprocess.check_output(args)
            # output: bytes = subprocess.check_output(args)
            # output: str = output.decode().rstrip()
            # info(f"{p} -> {output}")
        except subprocess.CalledProcessError:
            die(f"not in path: {p}")


def check_programs(yaml: Dict):
    progs = yaml.pop("in_path")
    if progs:
        check_programs_in_path(progs)

    if yaml:
        warn(f"unhandled requirements: {yaml}")


def check_host(host: str, yaml: Dict):
    reqs = yaml.get(host)
    if not reqs:
        return
    # print(f"{host} -> {reqs}")

    for (key, val) in reqs.items():
        if key == "apt":
            check_apt(val)
        elif key == "programs":
            check_programs(val)
        else:
            die(f"unknown key {key} (fragment: {reqs})")


def check_file(file: str, yaml):
    relpath = os.path.relpath(file, os.getcwd())
    info(f"checking requirements({relpath})")

    reqs = yaml.get("requirements")
    if not reqs:
        return

    check_host("generic", reqs)

    if hostenv.is_ubuntu():
        check_host("ubuntu", reqs)

    else:
        warn("requirements checking is not implemented for non-ubuntu hosts")


def check(conf):
    for (cf, yaml) in conf.project_conf.items():
        check_file(cf, yaml)


def collect(conf, host: str) -> Set[str]:
    def collect_packages_for_host(yaml: Dict):
        apt = yaml.get("apt")
        if apt:
            packages = apt.get("packages")
            if packages:
                return packages
        return []

    def collect_from_file(file: str, host: str):
        yaml = get_yaml(file)
        reqs = yaml.get("requirements")
        if reqs:
            host_reqs = reqs.get(host)
            if host_reqs:
                return collect_packages_for_host(host_reqs)
        else:
            return []

    res = []
    for cf in get_conf_files(conf):
        res += collect_from_file(cf, host)
    return set(res)
