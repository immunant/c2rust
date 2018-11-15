import difflib

def render_diff(files):
    file_names = sorted(files.keys())
    parts = []
    for f in file_names:
        old, new = files[f]
        parts.extend(difflib.unified_diff(
            old.splitlines(keepends=True),
            new.splitlines(keepends=True),
            fromfile='old/%s' % f,
            tofile='new/%s' % f))
        parts.append('\n\n')

    return ''.join(parts)
