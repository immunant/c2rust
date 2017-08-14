#!/usr/bin/env python

import os
import re
import sys
import errno
import shutil
import logging
import argparse

try:
    import plumbum as pb
except ImportError:
    # run `pip install plumbum` or `easy_install plumbum` to fix
    print >> sys.stderr, "error: python package plumbum is not installed."
    quit(errno.ENOENT)

SCRIPT_DIR = os.path.dirname(os.path.realpath(__file__))
LLVM_SRC = os.path.join(SCRIPT_DIR, 'llvm.src')
LLVM_BLD = os.path.join(SCRIPT_DIR, 'llvm.build')
LOG_FILE = os.path.realpath(__file__).replace(".py", ".log")
CBOR_URL = "https://codeload.github.com/01org/tinycbor/tar.gz/v0.4.1"
CBOR_ARCHIVE = os.path.join(SCRIPT_DIR, "tinycbor-0.4.1.tar.gz")
CBOR_SRC = os.path.basename(CBOR_ARCHIVE).replace(".tar.gz", "")
CBOR_SRC = os.path.join(SCRIPT_DIR, CBOR_SRC)
CBOR_PREFIX = os.path.join(SCRIPT_DIR, "tinycbor")

KEYSERVER = "pgpkeys.mit.edu"
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

MIN_PLUMBUM_VERSION = (1, 6, 3)

def die(emsg, ecode=1):
    logging.fatal("error: %s", emsg)
    quit(ecode)


def get_cmd_or_die(cmd):
    # FIXME: return wrapper that also does logging
    try:
        return pb.local[cmd]
    except pb.CommandNotFound:
        die("{} not in path".format(cmd), errno.ENOENT)


def download_llvm_sources():
    curl = get_cmd_or_die("curl")
    tar = get_cmd_or_die("tar")
    # on macOS, run `brew install gpg`
    gpg = get_cmd_or_die("gpg")

    # make sure we have the right public key installed
    gpg("--keyserver", KEYSERVER, "--recv-key", LLVM_PUBKEY)

    # download archives and signatures
    for (aurl, asig, afile, adir) in zip(
            LLVM_ARCHIVE_URLS,
            LLVM_SIGNATURE_URLS,
            LLVM_ARCHIVE_FILES,
            LLVM_ARCHIVE_DIRS):

        # download archive
        if not os.path.isfile(afile):
            logging.info("downloading %s", os.path.basename(afile))
            curl(aurl, "-o", afile)
        else:
            continue

        # download archive signature
        asigfile = afile + ".sig"
        if not os.path.isfile(asig):
            logging.debug("downloading %s", asigfile)
            curl(asig, "-o", asigfile)

        # check that archive matches signature
        try:
            expected = "Good signature from \"Tom Stellard <tom@stellard.net>\""
            logging.debug("checking signature of %s", os.path.basename(afile))
            retcode, stdout, stderr = gpg['--verify', asigfile, afile].run(retcode=None)
            if retcode:
                die("gpg signature check failed: gpg exit code " + str(retcode))
            if not expected in stderr:
                die("gpg signature check failed: expected signature not found")
        except pb.ProcessExecutionError as pee:
            die("gpg signature check failed: " + pee.message)

    # first extract llvm archive
    if not os.path.isdir(LLVM_SRC):
        logging.info("extracting %s", LLVM_ARCHIVE_FILES[0])
        tar("xf", LLVM_ARCHIVE_FILES[0])
        os.rename(LLVM_ARCHIVE_DIRS[0], LLVM_SRC)

    # then clang front end
    with pb.local.cwd(os.path.join(LLVM_SRC, "tools")):
        if not os.path.isdir("clang"):
            logging.info("extracting %s", LLVM_ARCHIVE_FILES[1])
            tar("xf", os.path.join(SCRIPT_DIR, LLVM_ARCHIVE_FILES[1]))
            os.rename(LLVM_ARCHIVE_DIRS[1], "clang")

        with pb.local.cwd("clang/tools"):
            if not os.path.isdir("extra"):
                logging.info("extracting %s", LLVM_ARCHIVE_FILES[2])
                tar("xf", os.path.join(SCRIPT_DIR, LLVM_ARCHIVE_FILES[2]))
                os.rename(LLVM_ARCHIVE_DIRS[2], "extra")


def configure_and_build_llvm():
    cmake = get_cmd_or_die("cmake")
    ninja = get_cmd_or_die("ninja")
    with pb.local.cwd(LLVM_BLD):
        if not os.path.isfile("build.ninja"):
            cmake["-G", "Ninja", LLVM_SRC,
                  "-Wno-dev",
                  "-DLLVM_BUILD_TESTS=ON",
                  "-DCMAKE_BUILD_TYPE=Release",
                  "-DLLVM_ENABLE_ASSERTIONS=1",
                  "-DLLVM_TARGETS_TO_BUILD=X86"] & pb.TEE
        else:
            logging.debug("found existing ninja.build, not running cmake")

        ninja & pb.TEE


def setup_logging():
    logging.basicConfig(
        filename=LOG_FILE,
        filemode='w',
        level=logging.DEBUG)

    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    logging.root.addHandler(console)


def ensure_dir(path):
    if not os.path.exists(path):
        logging.debug("creating dir %s", path)
        os.makedirs(path, mode=0744)
    if not os.path.isdir(path):
        die("%s is not a directory", path)


def update_cmakelists(filepath):
    if not os.path.isfile(filepath):
        die("not found: " + filepath, errno.ENOENT)

    cmakelists_commands = \
        """
        include_directories({prefix}/include)
        link_directories({prefix}/lib)
        add_subdirectory(ast-extractor)
        """.format(prefix=CBOR_PREFIX)
    indicator = "add_subdirectory(ast-extractor)"

    with open(filepath, "r") as fh:
        cmakelists = fh.readlines()
        add_commands = not any([indicator in l for l in cmakelists])
        logging.debug("add commands to %s: %s", filepath, add_commands)


    if add_commands:
        with open(filepath, "w+") as fh:
            fh.writelines(cmakelists_commands)
        logging.debug("added commands to %s", filepath)


def update_cbor_prefix(makefile):
    if not os.path.isfile(makefile):
        die("not found: " + makefile, errno.ENOENT)

    lines = []
    writeback = False
    with open(makefile, 'r') as fh:
        for line in fh.readlines():
            m = re.match(r'^\s*prefix\s*=\s*([^\s]+)', line)
            if m:
                logging.debug("tinycbor prefix: '%s'", m.group(1))
                prefix = m.group(1)
                writeback = prefix != CBOR_PREFIX
                lines.append("prefix = " + CBOR_PREFIX + os.linesep)
            else:
                lines.append(line)

    if writeback:
        logging.debug("updating tinycbor Makefile")
        with open(makefile, 'w') as fh:
            fh.writelines("".join(lines))


def install_tinycbor():
    """
    download, unpack, build, and install tinycbor.
    """
    if os.path.isdir(CBOR_PREFIX):
        logging.debug("skipping tinycbor installation")
        return

    # download
    if not os.path.isfile(CBOR_ARCHIVE):
        curl = get_cmd_or_die("curl")
        curl['-s', CBOR_URL, '-o', CBOR_ARCHIVE] & pb.TEE

    # unpack
    if not os.path.isdir(CBOR_SRC):
        tar = get_cmd_or_die("tar")
        tar['xf', CBOR_ARCHIVE] & pb.TEE

    # update install prefix
    update_cbor_prefix(os.path.join(CBOR_SRC, "Makefile"))

    # make && install
    with pb.local.cwd(CBOR_SRC):
        make = get_cmd_or_die("make")
        make & pb.TEE
        make('install')  # & pb.TEE


def integrate_ast_extractor():
    """
    link ast-extractor into $LLVM_SRC/tools/clang/tools/extra
    """
    src = os.path.join(SCRIPT_DIR, "ast-extractor")
    extractor_dest = os.path.join(
        LLVM_SRC, "tools/clang/tools/extra/ast-extractor")
    clang_tools_extra = os.path.abspath(
        os.path.join(extractor_dest, os.pardir))
    if not os.path.exists(extractor_dest):
        # NOTE: using os.symlink to emulate `ln -s` would be unwieldy
        ln = get_cmd_or_die("ln")
        with pb.local.cwd(clang_tools_extra):
            ln("-s", src)
    assert os.path.islink(extractor_dest), \
        "missing link: %s->%s" % (src, extractor_dest)

    cmakelists_path = os.path.join(clang_tools_extra, "CMakeLists.txt")
    update_cmakelists(cmakelists_path)


def parse_args():
    """
    define and parse command line arguments here.
    """
    desc = 'download dependencies for the AST extractor and built it.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('-c', '--clean', default=False,
                        action='store_true', dest='clean',
                        help='clean everything before building')
    return parser.parse_args()


def main():
    setup_logging()
    logging.debug("args: %s", " ".join(sys.argv))

    # earlier plumbum versions are missing features such as TEE
    if pb.__version__ < MIN_PLUMBUM_VERSION:
        err = "locally installed version {} of plumbum is too old.\n".format(pb.__version__)
        err += "please upgrade plumbum to version {} or later.".format(MIN_PLUMBUM_VERSION)
        die(err)

    args = parse_args()
    if args.clean:
        logging.info("cleaning previously downloaded and built files")
        shutil.rmtree(LLVM_SRC, ignore_errors=True)
        shutil.rmtree(LLVM_BLD, ignore_errors=True)
        shutil.rmtree(CBOR_PREFIX, ignore_errors=True)
        shutil.rmtree(CBOR_SRC, ignore_errors=True)

    # FIXME: allow env/cli override of LLVM_SRC, LLVM_VER, and LLVM_BLD
    # FIXME: check that cmake and ninja are installed
    # FIXME: option to build LLVM/Clang from master?

    ensure_dir(LLVM_BLD)

    download_llvm_sources()

    integrate_ast_extractor()

    install_tinycbor()

    configure_and_build_llvm()

if __name__ == "__main__":
    main()
