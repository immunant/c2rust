'''Input file parsing, to separate text from refactoring script and break the
script into commands.'''
from collections import namedtuple
import shlex

# A block of unprocessed Markdown text, represented as a list of lines with
# '\n' terminators intact.
Text = namedtuple('Text', ('lines',))
# A block of refactoring script.  `commands` is the refactoring commands, as a
# list of lists of arguments.  `raw` is the raw text as a list of lines, not
# including the `'```refactor'` ... `'```'` delimiters.
Script = namedtuple('Script', ('commands', 'raw'))

def split_commands(code: str) -> [[str]]:
    '''Parse a string as a sequence of shell words, then split those words into
    refactoring commands on `';'` separators.'''
    words = shlex.split(code)
    acc = []
    cmds = []

    for word in words:
        if word == ';':
            if len(acc) > 0:
                cmds.append(acc)
            acc = []
        else:
            acc.append(word)

    if len(acc) > 0:
        cmds.append(acc)

    return cmds


def parse_blocks(f):
    '''Parse a sequence of `Text` and `Script` blocks out of an input Markdown
    file.'''
    blocks = []

    cur = []
    in_refactor = False

    def commit():
        nonlocal cur
        if len(cur) > 0:
            if in_refactor:
                cmds = split_commands('\n'.join(cur))
                blocks.append(Script(cmds, cur))
            else:
                blocks.append(Text(cur))
        cur = []

    for line in f:
        sline = line.strip()
        if sline == '```refactor':
            commit()
            in_refactor = True
        elif sline == '```' and in_refactor:
            commit()
            in_refactor = False
        else:
            cur.append(line)

    return blocks
