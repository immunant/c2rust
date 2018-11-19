from collections import namedtuple
import pygments.formatters
import pygments.lexers
import pygments.token

from literate import diff


# This module works on labelings of the input text, represented as lists of
# `LabeledSpan`s.  
#
# Labeling invariants:
# - The list of LabeledSpans is sorted by `start`
# - The spans in the list are all non-overlapping
# - None of the spans are zero-length (start != end)
# - None of the `label`s is `None` (various functions use `None` as a sentinel
#   value indicating "no span covers this position")
LabeledSpan = namedtuple('LabeledSpan', ('start', 'end', 'label'))

# Operations on labelings

def iter_label_changes(ls, start_pos=0):
    '''Iterate over label changes, which occur at the boundaries of the labeled
    spans in `ls`.  Each item is a pair (pos, label), indicating that the
    "current label" changes to `label` starting at position `pos`.'''

    pos = start_pos
    for l in ls:
        if l.start == pos:
            # This span begins exactly at the end of the previous span.
            yield (pos, l.label)
        else:
            # There is a gap between the end of the previous span and the start
            # of the current one, covering pos .. l.start.
            yield (pos, None)
            yield (l.start, l.label)
        pos = l.end
    yield (pos, None)

def joined_label_changes(ls1, ls2):
    last_pos = 0

    def advance(it):
        try:
            pos, label = next(it)
            return pos, label, False
        except StopIteration:
            return None, None, True

    it1 = iter(iter_label_changes(ls1))
    pos1, label1, ended1 = advance(it1)
    prev_label1 = None

    it2 = iter(iter_label_changes(ls2))
    pos2, label2, ended2 = advance(it2)
    prev_label2 = None

    while not ended1 or not ended2:
        if pos1 is not None and pos2 is not None and pos1 == pos2:
            yield (pos1, label1, label2)
            prev_label1 = label1
            pos1, label1, ended1 = advance(it1)
            prev_label2 = label2
            pos2, label2, ended2 = advance(it2)
        elif pos2 is None or (pos1 is not None and pos1 < pos2):
            yield (pos1, label1, prev_label2)
            prev_label1 = label1
            pos1, label1, ended1 = advance(it1)
        elif pos1 is None or (pos2 is not None and pos2 < pos1):
            yield (pos2, prev_label1, label2)
            prev_label2 = label2
            pos2, label2, ended2 = advance(it2)

def zip_labels_with(f, ls1, ls2):
    ls3 = []

    last_pos = 0
    last_label = None
    for pos, label1, label2 in joined_label_changes(ls1, ls2):
        if last_label is not None:
            ls3.append(LabeledSpan(last_pos, pos, last_label))
        last_pos = pos
        last_label = f(label1, label2)

    return ls3

def zip_labels(ls1, ls2, intersect=False):
    if intersect:
        f = lambda x, y: (x, y) if x is not None and y is not None else None
    else:
        f = lambda x, y: (x, y) if x is not None or y is not None else None
    return zip_labels_with(f, ls1, ls2)


def cut_labels(orig_ls, cut_ls):
    '''Cut `orig_ls` into pieces, one for each labeled span in `cut_ls`.
    Returns a list of pairs of (cut LabeledSpan, piece).'''
    cur = []
    prev_pos = 0
    prev_orig_label = None
    prev_cut_pos = 0
    prev_cut_label = None

    for pos, orig_label, cut_label in joined_label_changes(orig_ls, cut_ls):
        if prev_orig_label is not None:
            cur.append(LabeledSpan(prev_pos, pos, prev_orig_label))
        if cut_label != prev_cut_label and len(cur) > 0:
            yield (LabeledSpan(prev_cut_pos, pos, prev_cut_label), cur)
            cur = []

        prev_pos = pos
        prev_orig_label = orig_label
        prev_cut_label = cut_label

    if len(cur) > 0:
        yield (LabeledSpan(prev_cut_pos, prev_pos, prev_cut_label), cur)

def label_runs(ls, key):
    cur = []
    start = 0
    end = 0
    last_k = key(None)

    for l in ls:
        k = key(l.label)
        if k != last_k:
            yield (LabeledSpan(start, end, last_k), cur)
            cur = []
            start, end = l.start, l.end
            last_k = k
        cur.append(l)


def label_lines(lines):
    '''Based on the length of the lines in list `lines`, compute a labeling for
    the original text that assigns each line its index (0-based) in `lines`.'''
    pos = 0
    ls = []
    for i, l in enumerate(lines):
        ls.append(LabeledSpan(pos, pos + len(l), i))
        pos += len(l)
    return ls


def label_highlights(text):
    '''Label `text` with Pygments highlighting tokens.'''
    lexer = pygments.lexers.get_lexer_by_name('rust')
    ls = []
    for start, token, token_text in lexer.get_tokens_unprocessed(text):
        if token == pygments.token.Whitespace:
            continue
        ls.append(LabeledSpan(start, start + len(token_text), token))
    return ls


# TODO: label_marks


# A collection of different labelings for a single text line.
LineParts = namedtuple('LineParts', ('span', 'highlight', 'diff_intra'))

def label_diff_intra(intra, line_span, label):
    return [LabeledSpan(
        line_span.start + s.start,
        line_span.start + s.end,
        label) for s in intra]

def line_builder(text, intra_label):
    lines = text.splitlines(keepends=True)
    line_ls = label_lines(lines)
    # Maps line index to line span
    line_spans = dict((l.label, l) for l in line_ls)
    # Maps line index to syntax-highlighting labeling
    highlight = dict((cut.label, ls)
            for (cut, ls) in cut_labels(label_highlights(text), line_ls))

    def build_line(idx, intra_spans):
        span = line_spans[idx]
        hl = highlight.get(idx, [])

        if intra_spans is None:
            intra = []
        else:
            intra = label_diff_intra(intra_spans, span, intra_label)

        return LineParts(span, hl, intra)

    return build_line


def diff_lines(old, new):
    hunks = diff.build_diff(old, new)

    build_old = line_builder(old, 'del')
    build_new = line_builder(new, 'ins')

    for blocks in hunks:
        yield None, None, None, None, None
        for b in blocks:
            if isinstance(b, diff.Context):
                assert len(b.old_lines) == len(b.new_lines)
                for i in range(len(b.old_lines)):
                    old_idx = b.old_lines.start + i
                    old = build_old(old_idx, None)
                    new_idx = b.new_lines.start + i
                    new = build_new(new_idx, None)
                    yield False, old_idx, old, new_idx, new

            elif isinstance(b, diff.Change):
                for i in range(max(len(b.old_lines), len(b.new_lines))):
                    if i < len(b.old_lines):
                        old_idx = b.old_lines.start + i
                        old = build_old(old_idx, None)
                    else:
                        old_idx, old = None, None
                    if i < len(b.new_lines):
                        new_idx = b.new_lines.start + i
                        new = build_new(new_idx, None)
                    else:
                        new_idx, new = None, None
                    yield True, old_idx, old, new_idx, new

            elif isinstance(b, diff.Intra):
                old = build_old(b.old_line, b.del_spans)
                new = build_new(b.new_line, b.ins_spans)
                yield True, b.old_line, old, b.new_line, new

            else:
                raise TypeError('expected block, but got %s' % (type(b),))

def render_line(text, line):
    line_ls = zip_labels(line.highlight, line.diff_intra)

    parts = []
    last_pos = line.span.start
    last_hl = None
    last_intra = None

    for pos, label in iter_label_changes(line_ls, start_pos=line.span.start):
        hl, intra = label if label is not None else (None, None)
        if hl in (pygments.token.Token, pygments.token.Text):
            hl = None

        intra_changed = intra != last_intra
        hl_changed = intra_changed or hl != last_hl

        parts.append(text[last_pos : pos])

        if hl_changed and last_hl is not None:
            parts.append('</span>')

        if intra_changed and last_intra is not None:
            parts.append('</span>')

        if intra_changed and intra is not None:
            parts.append('<span class="diff-intra-%s">' % intra)

        if hl_changed and hl is not None:
            parts.append('<span class="%s">' %
                    pygments.token.STANDARD_TYPES.get(hl))


        last_pos, last_hl, last_intra = pos, hl, intra

    parts.append(text[last_pos : line.span.end])
    return ''.join(parts)

def render_diff(files):
    file_names = sorted(files.keys())
    empty = True

    parts = []
    parts.append('<table class="diff highlight">\n')
    parts.append('<colgroup>')
    parts.append('<col width="50"><col><col width="50"><col>')
    parts.append('</colgroup>\n')
    for f in file_names:
        old, new = files[f]

        parts.append('<tr><td colspan="4" class="filename">%s</td></tr>' % f)

        first = False
        for change, old_idx, old_line, new_idx, new_line in diff_lines(old, new):
            empty = False
            old_cls = 'diff-old' if change else ''
            new_cls = 'diff-new' if change else ''

            if old_idx is None and new_idx is None:
                # This is a gap between hunks
                if not first:
                    parts.append('<tr><td colspan="4"><hr></td></tr>')
                first = False
                continue

            parts.append('<tr>')

            if old_idx is not None:
                parts.append('<td class="line-num %s">%d</td>' % (old_cls, old_idx + 1))
                parts.append('<td class="%s"><pre>' % old_cls)
                parts.append(render_line(old, old_line))
                parts.append('</pre></td>')
            else:
                parts.append('<td></td><td></td>')

            if new_idx is not None:
                parts.append('<td class="line-num %s">%d</td>' % (new_cls, new_idx + 1))
                parts.append('<td class="%s"><pre>' % new_cls)
                parts.append(render_line(new, new_line))
                parts.append('</pre></td>')
            else:
                parts.append('<td></td><td></td>')

            parts.append('</tr>\n')

    parts.append('</table>\n')

    if empty:
        return None

    return ''.join(parts)


def get_styles(fmt=None):
    fmt = fmt or pygments.formatters.get_formatter_by_name(
            'html', nowrap=True, style='monokai')

    parts = []
    parts.append('table.highlight { width: 800px; table-layout: fixed; }')
    parts.append('.highlight td { overflow-wrap: break-all; }')
    parts.append('.highlight td.line-num { text-align: right; }')
    parts.append('.highlight pre { display: inline; white-space: pre-wrap; }')

    # Diff highlight colors for use with `monokai` color scheme
    parts.append('.diff-old { background-color: #550000; }')
    parts.append('.diff-new { background-color: #004400; }')
    parts.append('.diff-intra-del { border: solid 1px #cc0000; }')
    parts.append('.diff-intra-ins { border: solid 1px #00cc00; }')

    # Colors for light-background color schemes, like `friendly`
    #parts.append('.diff-old { background-color: #ffcccc; }')
    #parts.append('.diff-new { background-color: #ccffcc; }')
    #parts.append('.diff-intra-del { border: solid 1px #880000; }')
    #parts.append('.diff-intra-ins { border: solid 1px #008800; }')

    parts.append(fmt.get_style_defs('.highlight'))
    return '\n'.join(parts) + '\n'


if __name__ == '__main__':
    def test_zip(ls1, ls2, ls3, intersect=False):
        result = zip_labels(ls1, ls2, intersect=intersect)
        if result != ls3:
            print('ls1 = %s' % ls1)
            print('ls2 = %s' % ls2)
            print('ls3 = %s' % ls3)
            print('result = %s' % result)
            assert False

    test_zip([
        LabeledSpan(10, 20, 'a'),
    ], [
        LabeledSpan(10, 20, 'b'),
    ], [
        LabeledSpan(10, 20, ('a', 'b')),
    ])

    test_zip([
        LabeledSpan(5, 15, 'a'),
    ], [
        LabeledSpan(10, 20, 'b'),
    ], [
        LabeledSpan(5, 10, ('a', None)),
        LabeledSpan(10, 15, ('a', 'b')),
        LabeledSpan(15, 20, (None, 'b')),
    ])

    test_zip([
        LabeledSpan(0, 10, 'a'),
    ], [
        LabeledSpan(10, 20, 'b'),
    ], [
        LabeledSpan(0, 10, ('a', None)),
        LabeledSpan(10, 20, (None, 'b')),
    ])

    test_zip([
        LabeledSpan(0, 10, 'a1'),
        LabeledSpan(10, 20, 'a2'),
    ], [
        LabeledSpan(5, 15, 'b'),
    ], [
        LabeledSpan(0, 5, ('a1', None)),
        LabeledSpan(5, 10, ('a1', 'b')),
        LabeledSpan(10, 15, ('a2', 'b')),
        LabeledSpan(15, 20, ('a2', None)),
    ])
