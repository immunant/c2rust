
import os
import re
import sys
import json
import errno
import distro
import psutil
import signal
import logging
import argparse
import platform
import multiprocessing

from typing import Optional, List, Callable

try:
    import plumbum as pb
except ImportError:
    # run `pip install plumbum` or `easy_install plumbum` to fix
    print("error: python package plumbum is not installed.", file=sys.stderr)
    sys.exit(errno.ENOENT)


class Colors:
    # Terminal escape codes
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    NO_COLOR = '\033[0m'


class Config:
    BUILD_SUFFIX = ""
    # use custom build directory suffix if requested via env. variable
    if os.getenv('C2RUST_BUILD_SUFFIX'):
        BUILD_SUFFIX = os.getenv('C2RUST_BUILD_SUFFIX')
    BUILD_TYPE = "release"

    NCPUS = str(multiprocessing.cpu_count())

    ROOT_DIR = os.path.dirname(os.path.realpath(__file__))
    ROOT_DIR = os.path.abspath(os.path.join(ROOT_DIR, os.pardir))
    BUILD_DIR = os.path.join(ROOT_DIR, 'build' + BUILD_SUFFIX)
    RREF_DIR = os.path.join(ROOT_DIR, 'c2rust-refactor')
    C2RUST_DIR = os.path.join(ROOT_DIR, 'c2rust')
    CROSS_CHECKS_DIR = os.path.join(ROOT_DIR, "cross-checks")
    REMON_SUBMOD_DIR = os.path.join(CROSS_CHECKS_DIR, 'ReMon')
    LIBFAKECHECKS_DIR = os.path.join(CROSS_CHECKS_DIR, "libfakechecks")
    LIBCLEVRBUF_DIR = os.path.join(REMON_SUBMOD_DIR, "libclevrbuf")
    EXAMPLES_DIR = os.path.join(ROOT_DIR, 'examples')
    AST_EXPO_DIR = os.path.join(ROOT_DIR, 'c2rust-ast-exporter')
    AST_EXPO_SRC_DIR = os.path.join(AST_EXPO_DIR, 'src')
    AST_EXPO_PRJ_DIR = os.path.join(AST_EXPO_DIR, 'xcode')
    RUST_CHECKS_DIR = os.path.join(CROSS_CHECKS_DIR, 'rust-checks')

    TRANSPILE_CRATE_DIR = os.path.join(ROOT_DIR, 'c2rust-transpile')
    REFACTOR_CRATE_DIR = os.path.join(ROOT_DIR, 'c2rust-refactor')
    AST_BUILDER_CRATE_DIR = os.path.join(ROOT_DIR, 'c2rust-ast-builder')
    AST_EXPORTER_CRATE_DIR = os.path.join(ROOT_DIR, 'c2rust-ast-exporter')
    BITFIELDS_CRATE_DIR = os.path.join(ROOT_DIR, 'c2rust-bitfields')
    XCHECK_PLUGIN_CRATE_DIR = os.path.join(RUST_CHECKS_DIR, 'rustc-plugin')
    XCHECK_RUNTIME_CRATE_DIR = os.path.join(RUST_CHECKS_DIR, 'runtime')
    XCHECK_DERIVE_CRATE_DIR = os.path.join(RUST_CHECKS_DIR, 'derive-macros')
    XCHECK_BACKEND_DYNAMIC_DLSYM_CRATE_DIR = os.path.join(RUST_CHECKS_DIR, 'backends', 'dynamic-dlsym')
    XCHECK_CONFIG_CRATE_DIR = os.path.join(RUST_CHECKS_DIR, 'config')
    MACROS_CRATE_DIR = os.path.join(ROOT_DIR, 'c2rust-macros')

    CBOR_PREFIX = os.path.join(BUILD_DIR, "tinycbor")

    LLVM_VER = "7.0.0"
    # make the build directory unique to the hostname such that
    # building inside a vagrant/docker environment uses a different
    # folder than building directly on the host.
    LLVM_ARCHIVE_URLS = [
        'http://releases.llvm.org/{ver}/llvm-{ver}.src.tar.xz',
        'http://releases.llvm.org/{ver}/cfe-{ver}.src.tar.xz',
        'http://releases.llvm.org/{ver}/clang-tools-extra-{ver}.src.tar.xz',
    ]
    # See http://releases.llvm.org/download.html#7.0.0
    LLVM_PUBKEY = "scripts/llvm-{ver}-key.asc".format(ver=LLVM_VER)
    LLVM_PUBKEY = os.path.join(ROOT_DIR, LLVM_PUBKEY)
    LLVM_SRC = os.path.join(BUILD_DIR, 'llvm-{ver}/src'.format(ver=LLVM_VER))
    LLVM_CFG_DIR = os.path.join(LLVM_SRC, 'cmake/modules')
    LLVM_BLD = os.path.join(
        BUILD_DIR, 'llvm-{ver}/build'.format(ver=LLVM_VER))
    LLVM_INSTALL = os.path.join(
        BUILD_DIR, 'llvm-{ver}/install'.format(ver=LLVM_VER))
    LLVM_BIN = os.path.join(LLVM_INSTALL, 'bin')

    CLANG_XCHECK_PLUGIN_SRC = os.path.join(CROSS_CHECKS_DIR,
                                           "c-checks", "clang-plugin")
    CLANG_XCHECK_PLUGIN_BLD = os.path.join(BUILD_DIR,
                                           'clang-xcheck-plugin')

    MIN_PLUMBUM_VERSION = (1, 6, 3)
    CC_DB_JSON = "compile_commands.json"

    CUSTOM_RUST_NAME = 'nightly-2019-04-12'
    # output of `rustup run $CUSTOM_RUST_NAME -- rustc --version`
    # CUSTOM_RUST_RUSTC_VERSION = "rustc 1.32.0-nightly (21f268495 2018-12-02)"

    """
    Reflect changes to all configuration variables that depend on LLVM_VER
    """
    def _init_llvm_ver_deps(self):
        self.LLVM_ARCHIVE_URLS = [s.format(ver=self.LLVM_VER)
                                  for s in Config.LLVM_ARCHIVE_URLS]
        self.LLVM_SIGNATURE_URLS = [s + ".sig" for s in self.LLVM_ARCHIVE_URLS]
        self.LLVM_ARCHIVE_FILES = [os.path.basename(s)
                                   for s in self.LLVM_ARCHIVE_URLS]
        self.LLVM_ARCHIVE_DIRS = [s.replace(".tar.xz", "")
                                  for s in self.LLVM_ARCHIVE_FILES]
        self.LLVM_ARCHIVE_FILES = [os.path.join(Config.BUILD_DIR, s)
                                   for s in self.LLVM_ARCHIVE_FILES]
        self.LLVM_PUBKEY = "scripts/llvm-{ver}-key.asc".format(ver=self.LLVM_VER)
        self.LLVM_PUBKEY = os.path.join(self.ROOT_DIR, self.LLVM_PUBKEY)
        self.LLVM_SRC = os.path.join(self.BUILD_DIR, 'llvm-{ver}/src'.format(ver=self.LLVM_VER))
        self.LLVM_CFG_DIR = os.path.join(self.LLVM_SRC, 'cmake/modules')
        self.LLVM_BLD = os.path.join(
            self.BUILD_DIR,
            'llvm-{ver}/build'.format(ver=self.LLVM_VER))
        self.LLVM_INSTALL = os.path.join(
            self.BUILD_DIR,
            'llvm-{ver}/install'.format(ver=self.LLVM_VER))
        self.LLVM_BIN = os.path.join(self.LLVM_INSTALL, 'bin')
        self.CLANG_XCHECK_PLUGIN_BLD = os.path.join(
            self.BUILD_DIR,
            'clang-xcheck-plugin')

    def __init__(self):
        self._init_llvm_ver_deps()
        self.TRANSPILER = None  # set in `update_args`
        self.RREF_BIN = None    # set in `update_args`
        self.C2RUST_BIN = None  # set in `update_args`
        self.TARGET_DIR = None  # set in `update_args`
        self.check_rust_toolchain()
        self.update_args()

    def check_rust_toolchain(self):
        """
        Sanity check that the value of self.CUSTOM_RUST_NAME matches
        the contents of self.ROOT_DIR/rust-toolchain.
        """
        toolchain_path = os.path.join(self.ROOT_DIR, "rust-toolchain")
        if os.path.exists(toolchain_path):
            with open(toolchain_path) as fh:
                toolchain_name = fh.readline().strip()
            emesg = "Rust version mismatch.\n"
            emesg += "\tcommon.py expects:       {}\n" \
                     .format(self.CUSTOM_RUST_NAME)
            emesg += "\trust-toolchain requests: {}\n".format(toolchain_name)
            assert self.CUSTOM_RUST_NAME == toolchain_name, emesg

    def update_args(self, args=None):
        build_type = 'debug' if args and args.debug else 'release'
        has_ver = args and hasattr(args, 'llvm_ver') and args.llvm_ver
        llvm_ver = args.llvm_ver if has_ver else self.LLVM_VER

        self.BUILD_TYPE = build_type
        self.LLVM_VER = llvm_ver
        # update dependent variables
        self._init_llvm_ver_deps()

        self.TRANSPILER = "target/{}/c2rust-transpile".format(build_type)
        self.TRANSPILER = os.path.join(self.ROOT_DIR, self.TRANSPILER)

        self.RREF_BIN = "target/{}/c2rust-refactor".format(build_type)
        self.RREF_BIN = os.path.join(self.ROOT_DIR, self.RREF_BIN)

        self.C2RUST_BIN = "target/{}/c2rust".format(build_type)
        self.C2RUST_BIN = os.path.join(self.ROOT_DIR, self.C2RUST_BIN)

        self.TARGET_DIR = "target/{}/".format(build_type)
        self.TARGET_DIR = os.path.join(self.ROOT_DIR, self.TARGET_DIR)

    @staticmethod
    def add_args(parser: argparse.ArgumentParser):
        """Add common command-line arguments that CommonGlobals understands to
        construct necessary paths.
        """
        dhelp = 'use debug build of toolchain (default build' \
                ' is release+asserts)'
        parser.add_argument('-d', '--debug', default=False,
                            action='store_true', dest='debug',
                            help=dhelp)


config = Config()


def update_or_init_submodule(submodule_path: str):
    git = get_cmd_or_die("git")
    invoke_quietly(git, "submodule", "update", "--init", submodule_path)
    logging.debug("updated submodule %s", submodule_path)


def get_rust_toolchain_libpath() -> str:
    return _get_rust_toolchain_path("lib")


def get_rust_toolchain_binpath() -> str:
    return _get_rust_toolchain_path("bin")


def _get_rust_toolchain_path(dirtype: str) -> str:
    """
    Ask rustc for the correct path to its {lib,bin} directory.
    """

    # If rustup is being used, it will respect the RUSTUP_TOOLCHAIN environment
    # variable, according to:
    # https://github.com/rust-lang/rustup.rs/blob/master/README.md#override-precedence
    #
    # If rustup is not being used, we can't control the toolchain; but rustc
    # will ignore this environment variable, so setting it is harmless.

    sysroot = pb.local["rustc"].with_env(
        RUSTUP_TOOLCHAIN=config.CUSTOM_RUST_NAME,
    )("--print", "sysroot")

    return os.path.join(sysroot.rstrip(), dirtype)


def on_x86() -> bool:
    """
    return true on x86-based hosts.
    """
    return platform.uname().machine in ['x86_64', 'i386', 'i686' 'amd64']


def on_mac() -> bool:
    """
    return true on macOS/OS X.
    """
    return 'Darwin' in platform.platform()


def on_linux() -> bool:
    if on_mac():
        return False
    elif on_ubuntu() or on_arch() or on_debian() or on_fedora():
        return True
    else:
        # neither on mac nor on a known distro
        assert False, "not sure"


def on_arch() -> bool:
    """
    return true on arch distros.
    """
    return distro.name() == "Arch Linux"



def on_ubuntu() -> bool:
    """
    return true on recent ubuntu linux distro.
    """
    return distro.name() == "Ubuntu"


def on_debian() -> bool:
    """
    return true on debian distro (and derivatives).
    """
    return distro.name().startswith("Debian") or distro.like() == "debian"


def on_fedora() -> bool:
    """
    return true on debian distro (and derivatives).
    """
    return distro.name() == "Fedora" or "fedora" in distro.like()


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


Command = pb.machines.LocalCommand


def get_cmd_or_die(cmd: str) -> Command:
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


def is_elf_exe(path):
    _file = pb.local.get('file')
    out = _file(path)
    return "LSB" in out and "ELF" in out and "Mach-O" not in out


def git_ignore_dir(path):
    """
    make sure directory has a `.gitignore` file with a wildcard pattern in it.
    """
    ignore_file = os.path.join(path, ".gitignore")
    if not os.path.isfile(ignore_file):
        with open(ignore_file, "w") as handle:
            handle.write("*\n")


def setup_logging(log_level=logging.INFO):
    logging.basicConfig(
        filename=sys.argv[0].replace(".py", ".log"),
        filemode='w',
        level=logging.DEBUG
    )

    console = logging.StreamHandler()
    console.setLevel(log_level)
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
    actual_version = rustup("run", config.CUSTOM_RUST_NAME, rustc["--version"])
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
    actual_version = rustup("run", config.CUSTOM_RUST_NAME, rustfmt_cmd)
    if expected_version_str not in actual_version:
        emsg = "expected version: {}\n"
        emsg = emsg + 9 * "." + "actual version: {}"
        emsg = emsg.format(expected_version_str, actual_version)
        die(emsg)


def ensure_clang_version(min_ver: List[int]):
    clang = get_cmd_or_die("clang")
    version = clang("--version")

    def _common_check(match):
        nonlocal version
        if match:
            version = match.group(1)
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


def get_ninja_build_type(ninja_build_file):
    signature = "# CMAKE generated file: DO NOT EDIT!" + os.linesep
    with open(ninja_build_file, "r") as handle:
        lines = handle.readlines()
        if not lines[0] == signature:
            die("unexpected content in ninja.build: " + ninja_build_file)
        r = re.compile(r'^#\s*Configuration:\s*(\w+)')
        for line in lines:
            m = r.match(line)
            if m:
                # print m.group(1)
                return m.group(1)
        die("missing content in ninja.build: " + ninja_build_file)


def export_ast_from(ast_expo: pb.commands.BaseCommand,
                    cc_db_path: str,
                    **kwargs) -> str:
    """
    run c2rust-ast-exporter for a single compiler invocation.

    :param ast_expo: command object representing c2rust-ast-exporter
    :param cc_db_path: path/to/compile_commands.json
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
        # prepare c2rust-ast-exporter arguments
        cc_db_dir = os.path.dirname(cc_db_path)
        args = ["-p", cc_db_dir, filepath]
        # this is required to locate system libraries

        # run c2rust-ast-exporter
        logging.info("exporting ast from %s", os.path.basename(filename))
        # log the command in a format that's easy to re-run
        export_cmd = str(ast_expo[args])
        logging.debug("export command:\n %s", export_cmd)
        ast_expo[args] & pb.FG  # nopep8
        cbor_outfile = filepath + ".cbor"
        assert os.path.isfile(cbor_outfile), "missing: " + cbor_outfile
        return cbor_outfile
    except pb.ProcessExecutionError as pee:
        if pee.retcode >= 0:
            mesg = os.strerror(pee.retcode)
        else:
            mesg = "Received signal: "
            mesg += signal.Signals(-pee.retcode).name

        logging.fatal("command failed: %s", ast_expo[args])
        die("AST export failed: " + mesg, pee.retcode)


def transpile(cc_db_path: str,
              filter: str = None,
              extra_transpiler_args: List[str] = [],
              emit_build_files: bool = True,
              output_dir: str = None,
              emit_modules: bool = False,
              main_module_for_build_files: str = None,
              cross_checks: bool = False,
              use_fakechecks: bool = False,
              cross_check_config: List[str] = [],
              incremental_relooper: bool = True,
              reorganize_definitions: bool = False) -> bool:
    """
    run the transpiler on all C files in a compile commands database.
    """
    c2rust = get_cmd_or_die(config.C2RUST_BIN)
    args = ['transpile', cc_db_path]
    args.extend(extra_transpiler_args)
    if emit_build_files:
        args.append('--emit-build-files')
    if output_dir:
        args.append('--output-dir')
        args.append(output_dir)
    if emit_modules:
        args.append('--emit-modules')
    if main_module_for_build_files:
        args.append('--main')
        args.append(main_module_for_build_files)
    if cross_checks:
        args.append('--cross-checks')
    if use_fakechecks:
        args.append('--use-fakechecks')
    if cross_check_config and cross_checks:
        args.append('--cross-check-config')
        for ccc in cross_check_config:
            args.append(ccc)
    if not incremental_relooper:
        args.append('--no-incremental-relooper')
    if reorganize_definitions:
        args.append('--reorganize-definitions')
    if filter:
        args.append('--filter')
        args.append(filter)

    logging.debug("translation command:\n %s", str(c2rust[args]))
    retcode, stdout, stderr = (c2rust[args]).run(retcode=None)
    logging.debug("stdout:\n%s", stdout)
    logging.debug("stderr:\n%s", stderr)

    return retcode == 0


def _get_gpg_cmd():
    # on macOS, run `brew install gpg`
    gpg = None
    try:
        # some systems install gpg v2.x as `gpg2`
        gpg = pb.local['gpg2']
    except pb.CommandNotFound:
        gpg = get_cmd_or_die("gpg")

    gpg.env = {'LANG': 'en'}  # request english output
    gpg_ver = gpg("--version")
    logging.debug("gpg version output:\n%s", gpg_ver)
    emsg = "{} in path is too old".format(gpg.executable.basename)
    assert "gpg (GnuPG) 1.4" not in gpg_ver, emsg

    return gpg


def install_sig(sigfile: str) -> None:
    gpg = _get_gpg_cmd()

    retcode, _, stderr = gpg['--import', sigfile].run(retcode=None)
    if retcode:
        logging.fatal(stderr)
        die('could not import gpg key: ' + sigfile, retcode)
    else:
        logging.debug(stderr)


def check_sig(afile: str, asigfile: str) -> None:
    gpg = _get_gpg_cmd()

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

    def _download_helper(url: str, ofile: str):
        if not os.path.isfile(ofile):
            logging.info("downloading %s", os.path.basename(ofile))
            follow_redirs = "-L"
            curl(url, follow_redirs, "--max-redirs", "20", "-o", ofile)

    _download_helper(aurl, afile)

    if not asig:
        return

    # download archive signature
    asigfile = afile + ".sig"
    _download_helper(asig, asigfile)

    check_sig(afile, asigfile)


class NonZeroReturn(Exception):
    pass
