import sys
import errno
import subprocess

from typing import List


class Colors(object):
    # Terminal escape codes
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    NO_COLOR = '\033[0m'


def die(emsg: str, status: int=errno.EINVAL):
    (red, nc) = (Colors.FAIL, Colors.NO_COLOR)
    print(f"{red}error:{nc} {emsg}", file=sys.stderr)
    exit(status)


def warn(warn: str):
    (yellow, nc) = (Colors.WARNING, Colors.NO_COLOR)
    print(f"{yellow}warning:{nc} {warn}", file=sys.stderr)


def info(imsg: str):
    (blue, nc) = (Colors.OKBLUE, Colors.NO_COLOR)
    print(f"{blue}info:{nc} {imsg}", file=sys.stdout)    
