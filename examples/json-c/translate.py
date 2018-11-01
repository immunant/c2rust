import json
import os
import sys
from plumbum.cmd import mv, mkdir, rename, sed, rustc, cargo, rm
from plumbum import local

# Path to the root of the json-c codebase
JSON_C_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), 'repo'))

sys.path.append(os.path.join(JSON_C_DIR, '../../../scripts'))
from common import *
import transpile


# List of rust-refactor commands to run.

def mk_select(script, mark='target'):
    return ['select', mark, script]

REFACTORINGS = [
    ['link_incomplete_types'],

    mk_select(r'''
        crate;
        desc(
            path(::arraylist::array_list) ||
            path(::json_object::json_object) ||
            path(::json_object_iterator::json_object_iterator) ||
            path(::json_tokener::json_tokener) ||
            path(::json_tokener::srec) ||
            path(::linkhash::lh_entry) ||
            path(::linkhash::lh_table) ||
            path(::printbuf::printbuf)
        );''') + [';', 'canonicalize_structs'],

    ['link_funcs'],

    mk_select('crate; desc(fn && pub);') + [';', 'wrap_api'],

    # Omit locale functions because the `libc`'s versions are unusable.
    # (`libc::locale_t` is defined incorrectly.)
    mk_select('crate; desc(foreign_item && fn && !name(".*locale.*"));') +
        [';', 'canonicalize_externs', 'libc'],

    mk_select(r'crate; desc(foreign_item && fn && name("__isnan(l|f)?"));') +
        [';', 'mark_uses', 'target',
            ';', 'rewrite_expr', 'marked!(__e)(__f)', '__f.is_nan() as i32'],
    mk_select(r'crate; desc(foreign_item && fn && name("__isinf(l|f)?"));') +
        [';', 'mark_uses', 'target',
            ';', 'rewrite_expr', 'marked!(__e)(__f)', '__f.is_infinite() as i32'],

    mk_select('crate; child(mod && !name("c_funcs")); desc(foreign_item);') +
        [';', 'canonicalize_externs', 'c_funcs'],
]




idiomize = get_cmd_or_die(config.RREF_BIN)

def run_idiomize(args, mode='inplace'):
    full_args = ['-r', mode] + args + [
            '--', 'src/lib.rs', '--crate-type=dylib',
            '--crate-name=json_c',
            '-L{rust_libdir}/rustlib/{triple}/lib/'.format(
                rust_libdir=get_rust_toolchain_libpath(),
                triple=get_host_triplet())]

    ld_lib_path = get_rust_toolchain_libpath()

    # don't overwrite existing ld lib path if any...
    if 'LD_LIBRARY_PATH' in local.env:
        ld_lib_path += ':' + local.env['LD_LIBRARY_PATH']

    # import ast
    with local.env(RUST_BACKTRACE='1',
                   LD_LIBRARY_PATH=ld_lib_path):
        with local.cwd(os.path.join(JSON_C_DIR, 'rust')):
            idiomize[full_args]()


def main():
    os.chdir(JSON_C_DIR)


    # Patch compile_commands to remove certain flags
    with open('compile_commands.json') as f:
        cc_json = json.load(f)

    for entry in cc_json:
        if 'arguments' not in entry:
            continue

        new_args = []
        for arg in entry['arguments']:
            if arg in ('-Werror', '-D_FORTIFY_SOURCE=2'):
                continue
            new_args.append(arg)

        entry['arguments'] = new_args

    with open('compile_commands.json', 'w') as f:
        json.dump(cc_json, f, indent=4)


    # Remove object files that will confuse `transpile`
    rm['-f', local.path('.') // '*.o', local.path('.libs') // '*.o']()


    # Actually translate
    with open('compile_commands.json', 'r') as f:
        transpile.transpile_files(f,
                emit_build_files=False,
                verbose=True)


    # Move rust files into rust/src
    mkdir['-vp', 'rust/src']()
    mv['-v', local.path('.') // '*.rs', 'rust/src/']()


    # Apply some fixes

    # 1. json-c uses `///` comments on some local assignment expressions, which
    # turn into `///` doc comments in the translated Rust.  But Rust doesn't
    # allow doc comments in those places.  We just globally replace `///` with
    # `//` for now.
    sed['-i', '-e', 's.///\+.//.g', local.path('rust/src') // '*.rs']()

    # 2. transpiler omits _i8 annotation on the translations of certain
    # string literals in places where the type can't be inferred.
    sed['-i', '-e', r'/errno_str:/s/&\[\([0-9]\+\),/\&[\1i8,/',
            'rust/src/strerror_override.rs']()


    for refactor_args in REFACTORINGS:
        print('REFACTOR: %r' % (refactor_args,))
        run_idiomize(refactor_args)


if __name__ == '__main__':
    main()
