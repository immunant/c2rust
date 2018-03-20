
import os
import re
import sys
import json
import errno
import psutil
import signal
import logging
import argparse
import platform
import multiprocessing

from typing import List

try:
    import plumbum as pb
except ImportError:
    # run `pip install plumbum` or `easy_install plumbum` to fix
    print("error: python package plumbum is not installed.", file=sys.stderr)
    sys.exit(errno.ENOENT)

NCPUS = str(multiprocessing.cpu_count())

# Terminal escape codes
OKBLUE = '\033[94m'
OKGREEN = '\033[92m'
WARNING = '\033[93m'
FAIL = '\033[91m'
NO_COLOUR = '\033[0m'

ROOT_DIR = os.path.dirname(os.path.realpath(__file__))
ROOT_DIR = os.path.abspath(os.path.join(ROOT_DIR, os.pardir))
DEPS_DIR = os.path.join(ROOT_DIR, 'dependencies')
RREF_DIR = os.path.join(ROOT_DIR, 'rust-refactor')
COMPILER_SUBMOD_DIR = os.path.join(RREF_DIR, 'compiler')
CROSS_CHECKS_DIR = os.path.join(ROOT_DIR, "cross-checks")
REMON_SUBMOD_DIR = os.path.join(CROSS_CHECKS_DIR, 'ReMon')
LIBCLEVRBUF_DIR = os.path.join(REMON_SUBMOD_DIR, "libclevrbuf")

AST_IMPO = os.path.join(ROOT_DIR, "ast-importer/target/debug/ast_importer")

CBOR_URL = "https://codeload.github.com/01org/tinycbor/tar.gz/v0.4.2"
CBOR_ARCHIVE = os.path.join(DEPS_DIR, "tinycbor-0.4.2.tar.gz")
CBOR_SRC = os.path.basename(CBOR_ARCHIVE).replace(".tar.gz", "")
CBOR_SRC = os.path.join(DEPS_DIR, CBOR_SRC)
CBOR_PREFIX = os.path.join(DEPS_DIR, "tinycbor.")
# use an install prefix unique to the host
CBOR_PREFIX += platform.node()  # returns hostname

BEAR_URL = "https://codeload.github.com/rizsotto/Bear/tar.gz/2.3.11"
BEAR_ARCHIVE = os.path.join(DEPS_DIR, "Bear-2.3.11.tar.gz")
BEAR_SRC = os.path.basename(BEAR_ARCHIVE).replace(".tar.gz", "")
BEAR_SRC = os.path.join(DEPS_DIR, BEAR_SRC)
BEAR_PREFIX = os.path.join(DEPS_DIR, "Bear.")
# use an install prefix unique to the host
BEAR_PREFIX += platform.node()  # returns hostname
BEAR_BIN = os.path.join(BEAR_PREFIX, "bin/bear")

LLVM_VER = "6.0.0"
LLVM_SRC = os.path.join(DEPS_DIR, 'llvm-{ver}/src'.format(ver=LLVM_VER))
LLVM_BLD = os.path.join(DEPS_DIR, 'llvm-{ver}/build.'.format(ver=LLVM_VER))
# make the build directory unique to the hostname such that
# building inside a vagrant/docker environment uses a different
# folder than building directly on the host.
LLVM_BLD += platform.node()  # returns hostname
LLVM_BIN = os.path.join(LLVM_BLD, 'bin')
LLVM_ARCHIVE_URLS = """
http://releases.llvm.org/{ver}/llvm-{ver}.src.tar.xz
http://releases.llvm.org/{ver}/cfe-{ver}.src.tar.xz
http://releases.llvm.org/{ver}/clang-tools-extra-{ver}.src.tar.xz
""".split("\n")
LLVM_ARCHIVE_URLS = [s.format(ver=LLVM_VER) for s in LLVM_ARCHIVE_URLS if s]
LLVM_SIGNATURE_URLS = [s + ".sig" for s in LLVM_ARCHIVE_URLS]
LLVM_ARCHIVE_FILES = [os.path.basename(s) for s in LLVM_ARCHIVE_URLS]
LLVM_ARCHIVE_DIRS = [s.replace(".tar.xz", "") for s in LLVM_ARCHIVE_FILES]
LLVM_ARCHIVE_FILES = [os.path.join(DEPS_DIR, s) for s in LLVM_ARCHIVE_FILES]

AST_EXTR = os.path.join(LLVM_BLD, "bin/ast-extractor")

MIN_PLUMBUM_VERSION = (1, 6, 3)
CMAKELISTS_COMMANDS = \
"""
add_subdirectory(ast-extractor)
""".format(prefix=CBOR_PREFIX)  # nopep8
CC_DB_JSON = "compile_commands.json"

# CUSTOM_RUST_URL = "git@github.com:rust-lang/rust"
# CUSTOM_RUST_PREFIX = os.path.join(DEPS_DIR, "rust")
# CUSTOM_RUST_REV = "cfcac37204c8dbdde192c1c9387cdbe663fe5ed5"
# NOTE: `rustup run nightly-2017-11-20 -- rustc --version` should output
# rustc 1.23.0-nightly (5041b3bb3 2017-11-19)
CUSTOM_RUST_NAME = 'nightly-2018-01-06'
CUSTOM_RUST_RUSTC_VERSION = "rustc 1.24.0-nightly (b98fd524e 2018-01-05)"


def have_rust_toolchain(name: str) -> bool:
    """
    Check whether name is output by `rustup show` on its own line.
    """
    rustup = get_cmd_or_die('rustup')
    lines = rustup('show').split('\n')
    return any([True for l in lines if l.startswith(name)])


def get_host_triplet() -> str:
    if on_linux():
        return "x86_64-unknown-linux-gnu"
    elif on_mac():
        return "x86_64-apple-darwin"
    else:
        assert False, "not implemented"


def get_rust_toolchain_libpath(name: str) -> str:
    """
    returns library path to custom rust libdir

    """
    if platform.architecture()[0] != '64bit':
        die("must be on 64-bit host")

    host_triplet = get_host_triplet()

    libpath = ".rustup/toolchains/{}-{}/lib/"
    libpath = libpath.format(CUSTOM_RUST_NAME, host_triplet)
    libpath = os.path.join(pb.local.env['HOME'], libpath)
    emsg = "custom rust compiler lib path missing: " + libpath
    assert os.path.isdir(libpath), emsg
    return libpath


def download_and_build_custom_rustc(args):
    """
    NOTE: we''re not using this function currently
    since it is faster and easier to pull the prebuilt
    binaries for the custom rust we need with rustup.
    """
    git = get_cmd_or_die('git')
    rustup = get_cmd_or_die('rustup')

    # check if rustup already lists c2rust custom toolchain
    # so we can avoid this time consuming step if we're not cleaning.
    if args.clean_all and have_rust_toolchain(CUSTOM_RUST_NAME):
        rustup['toolchain', 'uninstall', CUSTOM_RUST_NAME] & pb.FG
    elif have_rust_toolchain(CUSTOM_RUST_NAME):
        m = "skipping custom rust toolchain build step; already installed"
        logging.info(m)
        return

    assert on_linux(), "FIXME: set target_triple based on host os"
    target_triple = 'x86_64-unknown-linux-gnu'

    # disable host key checking to avoid prompts during automated builds
    with pb.local.env(GIT_SSH_COMMAND="ssh -o StrictHostKeyChecking=no"):
        # recursively update and (optionally) initialize submodules
        invoke(git, "submodule", "update", "--init", "--recursive",
               COMPILER_SUBMOD_DIR)

    with pb.local.cwd(COMPILER_SUBMOD_DIR):
        # seems that just updating submodule gives us the right version
        # invoke(git, 'reset', '--hard', CUSTOM_RUST_REV)

        x_py = pb.local['./x.py']
        if args.clean_all:
            x_py['clean'] & pb.FG

        if not os.path.isfile("config.toml"):
            configure = pb.local['./configure']
            configure & pb.FG

        x_py['build', '-j' + NCPUS] & pb.FG

    build_output = os.path.join(COMPILER_SUBMOD_DIR,
                                "build",
                                target_triple,
                                "stage2")
    assert os.path.isdir(build_output)
    rustup['toolchain', 'link', CUSTOM_RUST_NAME, build_output] & pb.FG


def on_mac() -> bool:
    """
    return true on macOS/OS X.
    """
    return 'Darwin' in platform.platform()


def on_linux() -> bool:
    if on_mac():
        return False
    elif on_ubuntu() or on_arch() or on_debian():
        return True
    else:
        # neither on mac nor on a known distro
        assert False, "not sure"


def on_arch() -> bool:
    """
    return true on arch distros.
    """
    distro, *_ = platform.linux_distribution()

    return distro == "arch"


def on_ubuntu() -> bool:
    """
    return true on recent ubuntu linux distro.
    """
    match = re.match(r'^.+Ubuntu-\d\d\.\d\d-\w+', platform.platform())
    return match is not None


def on_debian() -> bool:
    """
    return true on debian distro (and derivatives?).
    """
    distro, *_ = platform.linux_distribution()

    return distro == "debian"


def regex(raw: str):
    """
    Check that a string is a valid regex
    """

    try:
        return re.compile(raw)
    except re.error:
        msg = "only:{0} is not a valid regular expression".format(raw)
        raise argparse.ArgumentTypeError(msg)


def die(emsg, ecode=1):
    """
    log fatal error and exit with specified error code.
    """
    logging.fatal("error: %s", emsg)
    quit(ecode)


def est_parallel_link_jobs():
    """
    estimate the highest number of parallel link jobs we can
    run without causing the machine to swap. we conservatively
    estimate that a debug or release-with-debug-info link job
    requires approx 4GB of RAM and that all memory can be used.
    """
    mem_per_job = 4 * 1024**3
    mem_total = psutil.virtual_memory().total

    return int(mem_total / mem_per_job)


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


def git_ignore_dir(path):
    """
    make sure directory has a `.gitignore` file with a wildcard pattern in it.
    """
    ignore_file = os.path.join(path, ".gitignore")
    if not os.path.isfile(ignore_file):
        with open(ignore_file, "w") as handle:
            handle.write("*\n")


def setup_logging(logLevel=logging.INFO):
    logging.basicConfig(
        filename=sys.argv[0].replace(".py", ".log"),
        filemode='w',
        level=logging.DEBUG
    )

    console = logging.StreamHandler()
    console.setLevel(logLevel)
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


def ensure_rustc_version(expected_version_str: str):
    rustc = get_cmd_or_die("rustc")
    rustup = get_cmd_or_die("rustup")
    actual_version = rustup("run", CUSTOM_RUST_NAME, rustc["--version"])
    if expected_version_str not in actual_version:
        emsg = "expected version: {}\n"
        emsg = emsg + 9 * "." + "actual version: {}"
        emsg = emsg.format(expected_version_str, actual_version)
        die(emsg)


def ensure_rustfmt_version():
    expected_version_str = "0.10.0 ( ) DEPRECATED: use rustfmt-nightly\n"
    rustfmt = get_cmd_or_die("rustfmt")
    rustup = get_cmd_or_die("rustup")
    rustfmt_cmd = rustfmt["--force", "--version"]
    actual_version = rustup("run", CUSTOM_RUST_NAME, rustfmt_cmd)
    if expected_version_str not in actual_version:
        emsg = "expected version: {}\n"
        emsg = emsg + 9 * "." + "actual version: {}"
        emsg = emsg.format(expected_version_str, actual_version)
        die(emsg)


def ensure_clang_version(min_ver: List[int]):
    clang = get_cmd_or_die("clang")
    version = clang("--version")

    def _common_check(m):
        if m:
            version = m.group(1)
            # print(version)
            version = [int(d) for d in version.split(".")]
            emsg = "can't compare versions {} and {}".format(version, min_ver)
            assert len(version) == len(min_ver), emsg
            if version < min_ver:
                emsg = "clang version: {} < min version: {}"
                emsg = emsg.format(version, min_ver)
                die(emsg)
        else:
            logging.warning("unknown clang version: " + version)
            die("unable to identify clang version")

    if on_linux():
        m = re.search(r"clang\s+version\s([^\s-]+)", version)
        _common_check(m)
    elif on_mac():
        m = re.search(r"Apple\sLLVM\sversion\s([^\s-]+)", version)
        _common_check(m)
    else:
        assert False, "run this script on macOS or linux"


def get_system_include_dirs() -> List[str]:
    """
    note: assumes code was compiled with clang installed locally.
    """
    cc = get_cmd_or_die("clang")
    cmd = cc["-E", "-Wp,-v", "-"]
    _, _, stderr = cmd.run()
    dirs = stderr.split(os.linesep)
    # skip non-directory lines
    dirs = [l.strip() for l in dirs if len(l) and l[0] == ' ']
    # remove framework directory markers
    return [d.replace(" (framework directory)", "") for d in dirs]


def extract_ast_from(ast_extr: pb.commands.BaseCommand,
                     cc_db_path: str,
                     sys_incl_dirs: List[str],
                     **kwargs) -> str:
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

        logging.fatal("command failed: %s", ast_extr[args])
        die("sanity testing: " + mesg, pee.retcode)


def check_sig(afile: str, asigfile: str) -> None:
    # on macOS, run `brew install gpg`
    gpg = None
    try:
        # some systems install gpg v2.x as `gpg2`
        gpg = pb.local['gpg2']
    except pb.CommandNotFound:
        gpg = get_cmd_or_die("gpg")

    gpg_ver = gpg("--version")
    logging.debug("gpg version output:\n%s", gpg_ver)
    emsg = "{} in path is too old".format(gpg.executable.basename)
    assert "gpg (GnuPG) 1.4" not in gpg_ver, emsg

    def cleanup_on_failure(files: List[str]) -> None:
        for f in files:
            if os.path.isfile(f):
                os.remove(f)
            else:
                logging.warning("could not remove %s: not found.", f)

    if not os.path.isfile(afile):
        die("archive file not found: %s", afile)
    if not os.path.isfile(asigfile):
        die("signature file not found: %s", asigfile)

    # check that archive matches signature
    try:
        expected = "Good signature from "
        logging.debug("checking signature of %s", os.path.basename(afile))
        # --auto-key-retrieve means that gpg will try to download
        # the pubkey from a keyserver if it isn't on the local keyring.
        retcode, _, stderr = gpg['--keyserver-options',
                                 'auto-key-retrieve',
                                 '--verify',
                                 asigfile, afile].run(retcode=None)
        if retcode:
            cleanup_on_failure([afile, asigfile])
            logging.fatal(stderr)
            die("gpg signature check failed: gpg exit code " + str(retcode))
        if expected not in stderr:
            cleanup_on_failure([afile, asigfile])
            die("gpg signature check failed: expected signature not found")
    except pb.ProcessExecutionError as pee:
        cleanup_on_failure([afile, asigfile])
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


class NonZeroReturn(Exception):
    pass
