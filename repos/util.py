import sys
import errno


class Colors(object):
    # Terminal escape codes
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    NO_COLOR = '\033[0m'


def die(emsg: str, status: int=errno.EINVAL):
    print(emsg, file=sys.stderr)
    exit(status)


def warn(warn: str):
    print(f"warning: {warn}", file=sys.stderr)
