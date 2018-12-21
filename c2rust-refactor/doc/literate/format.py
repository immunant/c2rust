'''Run `rustfmt` on before-and-after file contents, to produce nicer looking
diffs.'''
import difflib
import os
import tempfile
from typing import List

from plumbum.cmd import rustfmt

from literate.annot import Span
from literate.file import File

def format_text_in_file(text: str, path: str) -> str:
    '''Run `rustfmt` on `text`, using `path` as a temporary file.  Returns the
    formatted text.'''
    with open(path, 'w') as f:
        f.write(text)

    rustfmt['--unstable-features', '--skip-children', path]()

    with open(path, 'r') as f:
        return f.read()

def format_files(all_files: List[File]):
    '''Format the `unformatted` text of every file in `all_files`, and set the
    formatted text for each one.'''
    with tempfile.TemporaryDirectory() as td:
        path = os.path.join(td, 'fmt.rs')

        fmt_text = {}
        for i, f in enumerate(all_files):
            print('formatting file %d (%s)' % (i, f.path))
            f.set_formatted(format_text_in_file(f.unformatted, path))


def init_fmt_map(f: File):
    '''Initialize a `File`'s `fmt_map` field, which describes the mapping from
    unformatted text positions to formatted ones.'''
    matching_spans = []
    sm = difflib.SequenceMatcher(a=f.unformatted, b=f.text)
    for tag, i1, i2, j1, j2 in sm.get_opcodes():
        if tag == 'equal':
            matching_spans.append((Span(i1, i2), j1))

    fmt_map_index = [s.start for s, pos in matching_spans]

    f.set_fmt_map(matching_spans, fmt_map_index)


