'''Input file parsing, to separate text from refactoring script and break the
script into commands.'''
from collections import namedtuple
import shlex

# A block of unprocessed Markdown text, represented as a list of lines with
# '\n' terminators intact.
Text = namedtuple('Text', ('lines',))
# A code block:
#
#       ```language,and-other,attrs
#       code goes here...
#       ```
Code = namedtuple('Code', ('attrs', 'lines'))

def parse_blocks(f):
    '''Parse a sequence of `Text` and `Code` blocks out of an input Markdown
    file.'''
    blocks = []

    cur = []
    # Inside a code block, this is a list of attrs from the start of the block.
    # Elsewhere, it's `None`.
    code_attrs = None

    def commit():
        nonlocal cur
        if len(cur) > 0:
            if code_attrs is not None:
                blocks.append(Code(code_attrs, cur))
            else:
                blocks.append(Text(cur))
        cur = []

    for line in f:
        sline = line.strip()
        if sline.startswith('```'):
            if code_attrs is not None:
                commit()
                code_attrs = None
            else:
                commit()
                code_attrs = sline[3:].strip().split()
        else:
            cur.append(line)

    commit()

    return blocks
