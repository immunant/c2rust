import json
import os
import sys
from plumbum.cmd import mv, mkdir, rename, sed, rustc, cargo, rm
from plumbum import local

# Path to the root of the json-c codebase
JSON_C_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), 'repo'))

sys.path.append(os.path.join(JSON_C_DIR, '../../../scripts'))
import transpile

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

    # 2. ast-importer omits _i8 annotation on the translations of certain
    # string literals in places where the type can't be inferred.
    sed['-i', '-e', r'/errno_str:/s/&\[\([0-9]\+\),/\&[\1i8,/',
            'rust/src/strerror_override.rs']()


if __name__ == '__main__':
    main()
