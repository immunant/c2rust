'''Run `rustfmt` on before-and-after file contents, to produce nicer looking
diffs.'''
import os
import tempfile

from plumbum.cmd import rustfmt

from literate import refactor

def format_text_in_file(text, path):
    with open(path, 'w') as f:
        f.write(text)

    rustfmt['--unstable-features', '--skip-children', path]()

    with open(path, 'r') as f:
        return f.read()

def format_files_in_temp_dir(blocks, temp_dir):
    path = os.path.join(temp_dir, 'fmt.rs')

    result = []
    for b in blocks:
        if isinstance(b, refactor.Text):
            result.append(b)
            continue
        elif isinstance(b, refactor.ScriptDiff):
            pass # handled below
        else:
            raise TypeError('expected Text or ScriptDiff, got %s' % (type(b),))

        fmt_text = {}
        for f, (old, new) in b.text.items():
            print('running rustfmt for %s' % f)
            fmt_old = format_text_in_file(old, path)
            fmt_new = format_text_in_file(new, path)
            fmt_text[f] = (fmt_old, fmt_new)

        result.append(refactor.ScriptDiff(b.commands, b.raw, fmt_text, b.nodes, b.marks))

    return result

def format_files(blocks):
    with tempfile.TemporaryDirectory() as td:
        return format_files_in_temp_dir(blocks, td)

