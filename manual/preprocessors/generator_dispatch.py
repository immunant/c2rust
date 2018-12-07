import json
import os
import re
import sys


def quote(args):
    return '{{#generate %s}}' % ' '.join(args)

def refactor_commands(args):
    assert len(args) == 0
    sys.path.append(os.path.join(os.path.dirname(__file__),
        '../../c2rust-refactor/doc'))
    import gen_command_docs
    return gen_command_docs.generate_commands()

KNOWN_GENERATORS = {
        'quote': quote,
        'refactor_commands': refactor_commands,
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

    [context, book] = json.load(sys.stdin)
    for section in book['sections']:
        replace_content(section)
    with open('tmp.json', 'w') as f:
        json.dump(book, f)
    json.dump(book, sys.stdout)
    exit(0)

if __name__ == '__main__':
    main()
