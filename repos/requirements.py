import yaml

import repos.hostenv as hostenv
from repos.util import *


def get_yaml(reqs: str) -> dict:
    with open(reqs, 'r') as stream:
        try:
            return yaml.safe_load(stream)
        except yaml.YAMLError as exc:
            die(str(exc))


def check_apt_package(yaml: dict):
    print(f"apt-packages: {yaml}")


def check_apt(yaml: dict):
    packages = yaml.pop("packages")
    if packages:
        check_apt_package(packages)
    if yaml:
        warn(f"unhandled requirements: {yaml}")


def check_programs_in_path(yaml: dict):
    print(f"programs-in-path: {yaml}")


def check_programs(yaml: dict):
    progs = yaml.pop("in_path")
    if progs:
        check_programs_in_path(progs)

    if yaml:
        print(f"unhandled requirements: {yaml}")


def check_host(host: str, yaml: dict):
    reqs = yaml.get(host)
    if not reqs:
        return
    print(f"{host} -> {reqs}")

    for (key, val) in reqs.items():
        if key == "apt":
            check_apt(val)
        elif key == "programs":
            check_programs(val)
        else:
            die(f"unknown key {key} (fragment: {reqs})")


def check(conf, reqs: str):
    if conf.verbose:
        print(f"checking dependencies in {reqs}")

    yaml = get_yaml(reqs)

    check_host("generic", yaml)

    if hostenv.is_ubuntu():
        check_host("ubuntu", yaml)

    else:
        eprint("requirements checking id not implemented for non-ubuntu hosts")
        exit(1)

