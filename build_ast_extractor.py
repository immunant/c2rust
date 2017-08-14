#!/usr/bin/env python

import os
import re
import sys
import errno
import logging
import plumbum as pb

SCRIPT_DIR = os.path.dirname(os.path.realpath(__file__))
LLVM_SRC = os.path.join(SCRIPT_DIR, 'llvm.src')
LLVM_BLD = os.path.join(SCRIPT_DIR, 'llvm.build')
LOG_FILE = os.path.realpath(__file__).replace(".py", ".log")
CBOR_PREFIX = os.path.join(SCRIPT_DIR, "tinycbor")
CBOR_URL = "https://codeload.github.com/01org/tinycbor/tar.gz/v0.4.1"
CBOR_ARCHIVE = "tinycbor-0.4.1.tar.gz"

KEYSERVER = "pgpkeys.mit.edu"
LLVM_PUBKEY = "8F0871F202119294"
LLVM_VER = "4.0.1"  # FIXME: allow env override
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
    # download, unpack, build, and install tinycbor
    cbor_dest = os.path.join(SCRIPT_DIR, CBOR_ARCHIVE)
    cbor_dir = os.path.basename(cbor_dest).replace(".tar.gz", "")
    cbor_dir = os.path.join(SCRIPT_DIR, cbor_dir)
    if not os.path.isfile(cbor_dest):
        curl = get_cmd_or_die("curl")
        tar = get_cmd_or_die("tar")

        curl['-s', CBOR_URL, '-o', cbor_dest] & pb.TEE
        # check whether tinycbor dir exists or not
        if not os.path.isdir(cbor_dir):
            tar['xf', cbor_dest] & pb.TEE
    update_cbor_prefix(os.path.join(cbor_dir, "Makefile"))
    with pb.local.cwd(cbor_dir):
        make = get_cmd_or_die("make")
        make & pb.TEE
        make('install')  # & pb.TEE


def integrate_ast_extractor():
    global clang_tools_extra
    # link ast-extractor into $LLVM_SRC/tools/clang/tools/extra
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


if __name__ == "__main__":
    setup_logging()
    logging.debug("args: %s", " ".join(sys.argv))

    # FIXME: add clean option to start all over
    # FIXME: allow env override of LLVM_SRC and LLVM_BLD
    # FIXME: check that cmake and ninja are installed

    ensure_dir(LLVM_BLD)

    download_llvm_sources()

    integrate_ast_extractor()

    install_tinycbor()

    configure_and_build_llvm()
