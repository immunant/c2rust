#!/usr/bin/env -S uv run

import json
import os
from plumbum.cmd import cargo
from plumbum import local
import re
import shlex
import sys
import tempfile


ROOT_DIR = os.path.join(os.path.dirname(__file__), '../..')


def quote(args):
    return '{{#generate %s}}' % ' '.join(args)


def refactor_commands(args):
    assert len(args) == 0
    sys.path.append(os.path.join(ROOT_DIR, 'c2rust-refactor/doc'))
    import gen_command_docs
    return gen_command_docs.generate_commands()


def literate(args):
    # Redirect stdout to stderr for `literate`
    real_stdout = sys.stdout
    sys.stdout = sys.stderr

    debug = os.environ.get('C2RUST_MANUAL_DEBUG_REFACTOR') == '1'

    # Make sure the c2rust binary we're about to run is up to date
    print('building c2rust...', file=sys.stderr)
    with local.cwd(os.path.join(ROOT_DIR)):
        if debug:
            cargo['build']()
        else:
            cargo['build', '--release']()

    if debug:
        args = ['-d'] + args

    if 'C2RUST_MANUAL_LITERATE_ARGS' in os.environ:
        args = shlex.split(os.environ['C2RUST_MANUAL_LITERATE_ARGS']) + args

    sys.path.append(os.path.join(ROOT_DIR, 'c2rust-refactor/doc'))
    with tempfile.TemporaryDirectory() as td:
        cmd_args = ['render'] + args + [os.path.join(td, 'out.md')]
        import literate
        literate.main(cmd_args)
        with open(os.path.join(td, 'out.md')) as f:
            result = f.read()

    sys.stdout = real_stdout
    return result


def translate_example(args):
    name, = args
    assert '/' not in name
    translate_py = os.path.join(ROOT_DIR, 'examples', name, 'translate.py')
    local[translate_py]()
    return '<!-- ran %s to translate %s -->' % (translate_py, name)


KNOWN_GENERATORS = {
    'quote': quote,
    'refactor_commands': refactor_commands,
    'literate': literate,
    'translate_example': translate_example,
}


DIRECTIVE_RE = re.compile(r'{{#([^}]+)}}')


def replace_content(section):
    if 'Chapter' not in section:
        return

    def repl(m):
        parts = m.group(1).strip().split()
        if len(parts) < 2 or parts[0] != 'generate':
            return m.group()

        gen = parts[1]
        args = parts[2:]
        if gen not in KNOWN_GENERATORS:
            raise KeyError('unknown generator %r' % gen)
        return KNOWN_GENERATORS[gen](args)

    content = section['Chapter']['content']
    content = DIRECTIVE_RE.sub(repl, content)
    section['Chapter']['content'] = content

    for item in section['Chapter']['sub_items']:
        replace_content(item)


def main():
    if len(sys.argv) > 1 and sys.argv[1] == 'supports':
        # We support all renderers
        exit(0)

    raw = sys.stdin.buffer.read()
    [context, book] = json.loads(raw.decode('utf-8'))
    for section in book['sections']:
        replace_content(section)

    # with open('tmp.json', 'w') as f:
    #     json.dump(book, f)
    json.dump(book, sys.stdout)
    exit(0)


if __name__ == '__main__':
    main()
