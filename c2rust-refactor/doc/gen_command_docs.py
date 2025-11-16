#!/usr/bin/env -S uv run

'''
Generate Markdown-formatted documentation for c2rust-refactor commands.  This
works by extracting from the c2rust-refactor source code any doc comment
starting with a line like this:

    # `foo` Command

The command name is taken from this header line, and the remainder of the doc
comment is included as the command description.
'''
import json
import os
import re
import sys


from plumbum.cmd import cargo
from plumbum import local, FG


def eprint(*args, **kwargs):
    kwargs.setdefault('file', sys.stderr)
    print(*args, **kwargs)


def load_analysis():
    '''Build c2rust-refactor with `-Z save-analysis`, then load the generated
    analysis JSON file.'''
    # Clear out the save-analysis directory so it's easy to find the file we
    # want.

    with local.cwd(os.path.join(os.path.dirname(__file__), '../..')):
        eprint('running `rustc -Z save-analysis` on c2rust-refactor...')
        cargo['rustc', '-p', 'c2rust-refactor','--lib',
                '--', '-Z', 'save-analysis'] & FG

        files = local.path('target/debug/deps/save-analysis') // 'libc2rust_refactor-*.json'

        newest_time = None
        newest_file = None
        for f in files:
            mtime = f.stat().st_mtime
            if newest_time is None or newest_time < mtime:
                newest_time = mtime
                newest_file = f

        assert newest_file is not None, 'analysis json not found'

        with open(newest_file) as f:
            eprint('loading analysis...')
            j = json.load(f)
            eprint('done (%d defs)' % len(j['defs']))
            return j


# Expect to see a header line of the form:  # `cmd_name` Command
HEADER_RE = re.compile(r'# `(.*)` Command')


def generate_commands():
    out = ''

    ana = load_analysis()

    # Find all documented commands

    cmds = []

    for d in ana['defs']:
        docs = d.get('docs')
        if docs is None:
            continue

        first, _, rest = docs.strip().partition('\n')
        m = HEADER_RE.match(first)
        if not m:
            continue

        name = m.group(1)
        content = rest
        cmds.append((name, content))

    eprint('found %d commands' % len(cmds))

    # Generate markdown output.

    cmds.sort(key=lambda x: x[0])

    out += '# Refactoring Commands\n\n'

    # Table of contents
    for name, _ in cmds:
        out += ' * [`%s`](#%s)\n' % (name, name)
    out += '\n'

    # One section per command
    for name, content in cmds:
        out += '## `%s`\n\n%s\n\n\n' % (name, content)

    return out

def main():
    print(generate_commands())


if __name__ == '__main__':
    main()
