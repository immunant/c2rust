
import os
import re
import sys
import json
import errno
import signal
import logging
import platform

from typing import List

try:
    import plumbum as pb
except ImportError:
    # run `pip install plumbum` or `easy_install plumbum` to fix
    print >> sys.stderr, "error: python package plumbum is not installed."
    sys.exit(errno.ENOENT)


ROOT_DIR = os.path.dirname(os.path.realpath(__file__))
ROOT_DIR = os.path.abspath(os.path.join(ROOT_DIR, os.pardir))
DEPS_DIR = os.path.join(ROOT_DIR, 'dependencies')

AST_IMPO = os.path.join(ROOT_DIR, "ast-importer/target/debug/ast-importer")

CBOR_URL = "https://codeload.github.com/01org/tinycbor/tar.gz/v0.4.1"
CBOR_ARCHIVE = os.path.join(DEPS_DIR, "tinycbor-0.4.1.tar.gz")
CBOR_SRC = os.path.basename(CBOR_ARCHIVE).replace(".tar.gz", "")
CBOR_SRC = os.path.join(DEPS_DIR, CBOR_SRC)
CBOR_PREFIX = os.path.join(DEPS_DIR, "tinycbor")

BEAR_URL = "https://codeload.github.com/rizsotto/Bear/tar.gz/2.3.6"
BEAR_ARCHIVE = os.path.join(DEPS_DIR, "Bear-2.3.6.tar.gz")
BEAR_SRC = os.path.basename(BEAR_ARCHIVE).replace(".tar.gz", "")
BEAR_SRC = os.path.join(DEPS_DIR, BEAR_SRC)
BEAR_PREFIX = os.path.join(DEPS_DIR, "Bear")
BEAR_BIN = os.path.join(BEAR_PREFIX, "bin/bear")

LLVM_SRC = os.path.join(ROOT_DIR, 'llvm.src')
LLVM_BLD = os.path.join(ROOT_DIR, 'llvm.build')
LLVM_BIN = os.path.join(LLVM_BLD, 'bin')
LLVM_PUBKEY = "8F0871F202119294"
LLVM_VER = "4.0.1"
LLVM_ARCHIVE_URLS = """
http://releases.llvm.org/{ver}/llvm-4.0.1.src.tar.xz
http://releases.llvm.org/{ver}/cfe-{ver}.src.tar.xz
http://releases.llvm.org/{ver}/clang-tools-extra-{ver}.src.tar.xz
http://releases.llvm.org/{ver}/clang-tools-extra-{ver}.src.tar.xz
""".split("\n")
LLVM_ARCHIVE_URLS = [s.format(ver=LLVM_VER) for s in LLVM_ARCHIVE_URLS if s]
LLVM_SIGNATURE_URLS = [s + ".sig" for s in LLVM_ARCHIVE_URLS]
LLVM_ARCHIVE_FILES = [os.path.basename(s) for s in LLVM_ARCHIVE_URLS]
LLVM_ARCHIVE_DIRS = [s.replace(".tar.xz", "") for s in LLVM_ARCHIVE_FILES]
LLVM_ARCHIVE_FILES = [os.path.join(DEPS_DIR, s) for s in LLVM_ARCHIVE_FILES]

AST_EXTR = os.path.join(LLVM_BLD, "bin/ast-extractor")

KEYSERVER = "pgpkeys.mit.edu"
MIN_PLUMBUM_VERSION = (1, 6, 3)
CMAKELISTS_COMMANDS = \
"""
include_directories({prefix}/include)
link_directories({prefix}/lib)
add_subdirectory(ast-extractor)
""".format(prefix=CBOR_PREFIX)  # nopep8
CC_DB_JSON = "compile_commands.json"


def on_mac() -> bool:
    """
    return true on macOS/OS X.
    """
    return 'Darwin' in platform.platform()


def on_ubuntu() -> bool:
    """
    return true on recent ubuntu linux distro.
    """
    match = re.match(r'^.+Ubuntu-\d\d\.\d\d-\w+', platform.platform())
    return match is not None


def die(emsg, ecode=1):
    """
    log fatal error and exit with specified error code.
    """
    logging.fatal("error: %s", emsg)
    quit(ecode)


def invoke(cmd, *arguments):
    return _invoke(True, cmd, *arguments)


def invoke_quietly(cmd, *arguments):
    return _invoke(False, cmd, *arguments)


def _invoke(console_output, cmd, *arguments):
    try:
        if console_output:
            retcode, stdout, stderr = cmd[arguments] & pb.TEE()
        else:
            retcode, stdout, stderr = cmd[arguments].run()

        if stdout:
            logging.debug("stdout from %s:\n%s", cmd, stdout)
        if stderr:
            logging.debug("stderr from %s:\n%s", cmd, stderr)

        return retcode, stdout, stderr
    except pb.ProcessExecutionError as pee:
        msg = "cmd exited with code {}: {}".format(pee.retcode, cmd[arguments])
        logging.critical(pee.stderr)
        die(msg, pee.retcode)


def get_cmd_or_die(cmd):
    """
    lookup named command or terminate script.
    """
    try:
        return pb.local[cmd]
    except pb.CommandNotFound:
        die("{} not in path".format(cmd), errno.ENOENT)


def ensure_dir(path):
    if not os.path.exists(path):
        logging.debug("creating dir %s", path)
        os.makedirs(path, mode=0o744)
    if not os.path.isdir(path):
        die("%s is not a directory", path)


def setup_logging():
    logging.basicConfig(
        filename=sys.argv[0].replace(".py", ".log"),
        filemode='w',
        level=logging.DEBUG)

    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    logging.root.addHandler(console)


def binary_in_path(binary_name) -> bool:
    try:
        # raises CommandNotFound exception if not available.
        _ = pb.local[binary_name]
        return True
    except pb.CommandNotFound:
        return False


def json_pp_obj(json_obj) -> str:
    return json.dumps(json_obj,
                      sort_keys=True,
                      indent=2,
                      separators=(',', ': '))


def ensure_clang_version(min_ver: List[int]):
    clang = get_cmd_or_die("clang")
    version = clang("--version")
    m = re.search("clang\s+version\s([^\s]+)", version)
    if m:
        version = m.group(1)
        version = version[:version.find("-")]
        # print(version)
        version = [int(d) for d in version.split(".")]
        emsg = "can't compare versions {} and {}".format(version, min_ver)
        assert len(version) == len(min_ver), emsg
        if version < min_ver:
            emsg = "clang version: {} < min version: {}".format(version, min_ver)
            die(emsg)
    else:
        die("unable to identify clang version")


def get_system_include_dirs() -> List[str]:
    """
    note: assumes code was compiled with clang installed locally.
    """
    cc = get_cmd_or_die("clang") 
    cmd = cc["-E", "-Wp,-v", "-"]
    _, _, stderr = cmd.run()
    dirs = stderr.split(os.linesep)
    return [l.strip() for l in dirs if len(l) and l[0] == ' ']


def extract_ast_from(ast_extr: pb.commands.BaseCommand,
                     cc_db_path: str,
                     sys_incl_dirs: List[str],
                     **kwargs) -> None:
    """
    run ast-extractor for a single compiler invocation.

    :param ast_extr: command object representing ast-extractor
    :param cc_db_path: path/to/compile_commands.json
    :param sys_incl_dirs: list of system include directories
    :return: path to generated cbor file.
    """
    # keys = ['arguments', 'directory', 'file']
    keys = ['directory', 'file']  # 'arguments' is not required
    try:
        dir, filename = [kwargs[k] for k in keys]
        filepath = os.path.join(dir, filename)
    except KeyError:
        die("couldn't parse " + cc_db_path)

    if not os.path.isfile(filepath):
        die("missing file " + filepath)
    try:
        # prepare ast-extractor arguments
        cc_db_dir = os.path.dirname(cc_db_path)
        args = ["-p", cc_db_dir, filepath]
        # this is required to locate system libraries
        args += ["-extra-arg=-I" + i for i in sys_incl_dirs]

        # run ast-extractor
        logging.info("extracting ast from %s", os.path.basename(filename))
        ast_extr[args] & pb.TEE  # nopep8
        cbor_outfile = filepath + ".cbor"
        assert os.path.isfile(cbor_outfile), "missing: " + cbor_outfile
        return cbor_outfile
    except pb.ProcessExecutionError as pee:
        if pee.retcode >= 0:
            mesg = os.strerror(pee.retcode)
        else:
            mesg = "Received signal: "
            mesg += signal.Signals(-pee.retcode).name

        logging.fatal("command failed: %s", 
                      ast_extr["-p", cc_db_dir, filename])
        die("FAIL sanity testing: " + mesg, pee.retcode)


def check_sig(afile: str, asigfile: str) -> None:
    # on macOS, run `brew install gpg`
    gpg = get_cmd_or_die("gpg")

    # make sure we have the right public key installed
    gpg("--keyserver", KEYSERVER, "--recv-key", LLVM_PUBKEY)

    # check that archive matches signature
    try:
        expected = "Good signature from \"Tom Stellard <tom@stellard.net>\""
        logging.debug("checking signature of %s", os.path.basename(afile))
        retcode, _, stderr = gpg['--verify', asigfile, afile].run(retcode=None)
        if retcode:
            die("gpg signature check failed: gpg exit code " + str(retcode))
        if expected not in stderr:
            die("gpg signature check failed: expected signature not found")
    except pb.ProcessExecutionError as pee:
        die("gpg signature check failed: " + pee.message)


def download_archive(aurl: str, afile: str, asig: str = None):
    curl = get_cmd_or_die("curl")

    # download archive
    if not os.path.isfile(afile):
        logging.info("downloading %s", os.path.basename(afile))
        curl(aurl, "-o", afile)

    if not asig:
        return

    # download archive signature
    asigfile = afile + ".sig"
    if not os.path.isfile(asig):
        logging.debug("downloading %s", asigfile)
        curl(asig, "-o", asigfile)

    check_sig(afile, asigfile)