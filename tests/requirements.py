import os
import yaml
import subprocess

import tests.hostenv as hostenv
from tests.util import *


def get_yaml(reqs: str) -> dict:
    with open(reqs, 'r') as stream:
        try:
            return yaml.safe_load(stream)
        except yaml.YAMLError as exc:
            die(str(exc))


def check_apt_package(yaml: List[str]):
    assert isinstance(yaml, list), "expected list of apt packages"
    for p in yaml:
        args = ["dpkg", "-l", p]

        try:
            output: bytes = subprocess.check_output(args)
        except subprocess.CalledProcessError:
            die(f"package not installed: {p}")

        output: str = output.decode()
        last: str = output.splitlines()[-1]
        expected = f"ii  {p}"
        if not last.startswith(expected):
            die(f"package not (properly) installed: {p}")


def check_apt(yaml: dict):
    packages = yaml.pop("packages")
    if packages:
        check_apt_package(packages)
    if yaml:
        warn(f"unhandled requirements: {yaml}")


def check_programs_in_path(yaml: dict):
    assert isinstance(yaml, list), "expected list of apt packages"

    for p in yaml:
        args = ["which", p]
        try:
            output: bytes = subprocess.check_output(args)
            output: str = output.decode().rstrip()
            # info(f"{p} -> {output}")
        except subprocess.CalledProcessError:
            die(f"not in path: {p}")


def check_programs(yaml: dict):
    progs = yaml.pop("in_path")
    if progs:
        check_programs_in_path(progs)

    if yaml:
        warn(f"unhandled requirements: {yaml}")


def check_host(host: str, yaml: dict):
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


def check(conf, reqs: str):
    relpath = os.path.relpath(reqs, os.getcwd())
    info(f"checking requirements({relpath})")

    yaml = get_yaml(reqs)

    check_host("generic", yaml)

    if hostenv.is_ubuntu():
        check_host("ubuntu", yaml)

    else:
        warn("requirements checking id not implemented for non-ubuntu hosts")
