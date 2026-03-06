
import os

import lit.formats

from lit.llvm import llvm_config

# Configuration

config.name = "cross-check-tests"

config.test_format = lit.formats.ShTest(not llvm_config.use_lit_shell)

config.suffixes = ['.c', '.cc', '.cpp']

config.excludes = ['Inputs']

# Disabled for now, test_source_root is set in lit.site.cfg.py.in
#config.test_source_root = os.path.dirname(__file__)

llvm_config.use_default_substitutions()
llvm_config.use_clang()

xcheck_include_path = os.path.abspath(
        os.path.join(config.test_source_root, os.pardir, "include"))
xcheck_plugin_so = os.path.abspath(
        os.path.join(config.test_exec_root, os.pardir,
                     "plugin", "CrossChecks.so"))
clang_xcheck_Xclang_args = [
        "-load", xcheck_plugin_so,
        "-add-plugin", "crosschecks"
        ]
clang_xcheck_args = " " + " ".join("-Xclang " + x for x in clang_xcheck_Xclang_args)
clang_xcheck_args += "-std=c11 "
clang_xcheck_args += "-I{} ".format(xcheck_include_path)
clang_xcheck_args += "-ffunction-sections "
clang_xcheck_args += "-fuse-ld=gold -Wl,--gc-sections,--icf=safe "

# Path to libruntime.a, used for %xcheck_runtime
xcheck_runtime_lib = os.path.abspath(
        os.path.join(config.test_exec_root, os.pardir,
                     "runtime", "libruntime.a"))

# Contents of %fakechecks substitution used to link in libfakechecks
fakechecks_dir = os.path.abspath(
        os.path.join(config.test_source_root, os.pardir, os.pardir,
                     os.pardir, "libfakechecks"))
fakechecks_args = " -L{fakechecks_dir} -lfakechecks"\
                  " -Wl,--rpath,{fakechecks_dir} ".format(
                          fakechecks_dir=fakechecks_dir)

config.substitutions.append(("%clang_xcheck", config.clang + clang_xcheck_args))
config.substitutions.append(("%xcheck_runtime", xcheck_runtime_lib))
config.substitutions.append(("%fakechecks", fakechecks_args))
