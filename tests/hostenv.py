import distro


def is_docker() -> bool:
    return path.isfile("/.dockerenv")


def is_ubuntu() -> bool:
    return distro.name() == "Ubuntu"


def is_ubuntu_1904() -> bool:
    return is_ubuntu() and distro.version() == "19.04"


def is_ubuntu_1804() -> bool:
    return is_ubuntu() and distro.version() == "18.04"


# def is_ubuntu_1404():
#     return 'Ubuntu-14.04-trusty' in platform()

def is_centos() -> bool:
    return distro.name() == "CentOS"


def is_debian() -> bool:
    return distro.name().startswith("Debian")


def has_apt_package(name: str) -> bool:
    return True
