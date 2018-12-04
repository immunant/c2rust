'''Input file parsing, to separate text from refactoring script and break the
script into commands.'''
from typing import List, Union, NamedTuple

class Text(NamedTuple):
    '''A block of unprocessed markdown.'''

    lines: List[str]
    '''The lines of markdown input, with `\n` terminators intact.'''

class Code(NamedTuple):
    '''A markdown fenced code block.

        ```language and-other attrs
        code goes here...
        ```
    '''

    attrs: List[str]
    '''The attributes from the opening line of the block, split on
    whitespace.'''

    lines: List[str]
    '''The content of the block, not including the opening and closing
    lines.'''

Block = Union[Text, Code]

def parse_blocks(f) -> List[Block]:
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
