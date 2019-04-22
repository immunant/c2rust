import re
from platform import platform


def is_docker() -> bool:
    return path.isfile("/.dockerenv")


def is_ubuntu() -> bool:
    m = re.match(r'^.+Ubuntu-\d\d\.\d\d-\w+', platform())
    return m is not None


def is_ubuntu_1904() -> bool:
    return 'Ubuntu-19.04-disco' in platform()


def is_ubuntu_1804() -> bool:
    return 'Ubuntu-18.04-bionic' in platform()


# def is_ubuntu_1404():
#     return 'Ubuntu-14.04-trusty' in platform()

def is_centos() -> bool:
    m = re.match(r'^.+centos-\d\.\d', platform())
    return m is not None


def is_debian() -> bool:
    return 'debian' in platform()


def has_apt_package(name: str) -> bool:
    return True
