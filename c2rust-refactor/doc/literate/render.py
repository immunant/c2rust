from collections import namedtuple
import html
import pygments.formatters
import pygments.lexers
import pygments.token

from literate.annot import Span, fill_annot, cut_annot
from literate.file import File, Diff
from literate.points import Point, map_points, merge_points, annot_starts, annot_ends
import literate.diff
import literate.highlight
import literate.marks


def mark_class(f: File, node_id: int) -> str:
    added, removed, kept = f.mark_labels.get(node_id, ((), (), ()))

    if len(added) == 0 and len(removed) == 0:
        return 'kept'
    elif len(added) > 0 and len(removed) == 0 and len(kept) == 0:
        return 'ins'
    elif len(removed) > 0 and len(added) == 0 and len(kept) == 0:
        return 'del'
    else:
        return 'chg'

def mark_desc(f: File, node_id: int) -> str:
    m = f.marks[node_id]

    if m.id == 0xffffffff:
        id_str = '#DUMMY'
    elif (m.id & 0x80000000) != 0:
        id_str = '#NEW-%d' % (m.id & ~0x80000000)
    else:
        id_str = '#%d' % m.id

    if m.name is not None and m.name != '':
        mark_str = '%s %s "%s"' % (m.kind, id_str, m.name)
    else:
        mark_str = '%s %s' % (m.kind, id_str)

    added, removed, kept = f.mark_labels.get(node_id, ((), (), ()))
    parts = []
    if len(added) > 0:
        parts.append('added %s' % ', '.join(repr(l) for l in added))
    if len(removed) > 0:
        parts.append('removed %s' % ', '.join(repr(l) for l in removed))
    if len(kept) > 0:
        parts.append('kept %s' % ', '.join(repr(l) for l in kept))

    return '%s: %s' % (mark_str, '; '.join(parts))

def render_line(line, f):
    parts = []
    def mark_marker(m, start):
        if start:
            sym = '&#x25b6'
        else:
            sym = '&#x25c0'
        parts.append('<a class="mark-%s" title="%s">%s</a>' %
                (mark_class(f, m), html.escape(mark_desc(f, m)), sym))


    if line.intra is not None:
        intra_start = annot_starts(line.intra)
        intra_end = annot_ends(line.intra)
    else:
        intra_start = []
        intra_end = []

    highlight_start = annot_starts(line.highlight)
    highlight_end = annot_ends(line.highlight)

    marks_start = line.mark_starts
    marks_end = line.mark_ends


    events = merge_points(
            map_points(highlight_end, lambda l: ('hl_e', l)),
            map_points(intra_end, lambda l: ('i_e', l)),
            map_points(marks_end, lambda l: ('m_e', l)),
            map_points(marks_start, lambda l: ('m_s', l)),
            map_points(intra_start, lambda l: ('i_s', l)),
            map_points(highlight_start, lambda l: ('hl_s', l)),
            )

    last_pos = 0

    if line.hunk_start_marks:
        for m in sorted(line.hunk_start_marks):
            mark_marker(m, True)

            # These hunk-start markers often appear before the whitespace
            # of indented lines.  To avoid throwing off the indentation, we let
            # them "eat up" as much whitespace as is available.  Any start/end
            # events in the eaten whitespace will be pushed back until after
            # the markers.  (Hopefully there are not many events inside a
            # purely whitespace span.)
            if last_pos < len(line.text) and line.text[last_pos] == ' ':
                last_pos += 1

    for p in events:
        if p.pos > last_pos:
            if p.pos == len(line.text) and line.text.endswith('\n'):
                # Avoid emitting a \n followed by a .mark-end indicator, which
                # would force the <pre> onto two lines.
                parts.append(line.text[last_pos : -1])
            else:
                parts.append(line.text[last_pos : p.pos])
        last_pos = p.pos

        kind, label = p.label

        if kind in 'm_s':
            for m in sorted(label):
                mark_marker(m, True)
        elif kind == 'm_e':
            for m in sorted(label):
                mark_marker(m, False)
        elif kind == 'i_s':
            parts.append('<span class="diff-intra-%s">' % label)
        elif kind == 'i_e':
            parts.append('</span>')
        elif kind == 'hl_s':
            if label not in (pygments.token.Token, pygments.token.Text):
                parts.append('<span class="%s">' %
                        pygments.token.STANDARD_TYPES.get(label))
        elif kind == 'hl_e':
            if label not in (pygments.token.Token, pygments.token.Text):
                parts.append('</span>')

    if len(line.text) > last_pos:
        parts.append(line.text[last_pos:])

    if line.hunk_end_marks:
        for m in sorted(line.hunk_end_marks):
            mark_marker(m, False)

    return ''.join(parts)

def prepare_files(files: [File]):
    for f in files:
        literate.highlight.highlight_file(f)
        literate.marks.mark_file(f)

def make_diff(f1: File, f2: File) -> Diff:
    d = literate.diff.diff_files(f1.copy(), f2.copy())
    from pprint import pprint
    pprint(d.blocks)
    literate.diff.build_diff_hunks(d)
    literate.diff.build_output_lines(d)
    literate.marks.init_hunk_boundary_marks(d)
    literate.marks.init_mark_status(d)
    return d

def render_diff(old_cs, new_cs):
    file_names = sorted(new_cs.files.keys())
    empty = True

    parts = []
    parts.append('<table class="diff highlight">\n')
    parts.append('<colgroup>')
    parts.append('<col width="50"><col><col width="50"><col>')
    parts.append('</colgroup>\n')
    for f in file_names:
        # `make_diff` copies the files, then updates the copies.  We want
        # references to the new copies only.
        diff = make_diff(old_cs.files[f], new_cs.files[f])
        old = diff.old_file
        new = diff.new_file

        if len(diff.hunks) > 0:
            empty = False

        parts.append('<tr><td colspan="4" class="filename">%s</td></tr>' % f)

        first = True
        for hunk in diff.hunks:
            if not first:
                parts.append('<tr><td colspan="4"><hr></td></tr>')
            first = False

            for ol in hunk.output_lines:
                old_cls = 'diff-old' if ol.changed else ''
                new_cls = 'diff-new' if ol.changed else ''

                parts.append('<tr>')

                if ol.old_line is not None:
                    parts.append('<td class="line-num %s">%d</td>' %
                            (old_cls, ol.old_line + 1))
                    parts.append('<td class="%s"><pre>' % old_cls)
                    parts.append(render_line(old.lines[ol.old_line], old))
                    parts.append('</pre></td>')
                else:
                    parts.append('<td></td><td></td>')

                if ol.new_line is not None:
                    parts.append('<td class="line-num %s">%d</td>' %
                            (new_cls, ol.new_line + 1))
                    parts.append('<td class="%s"><pre>' % new_cls)
                    parts.append(render_line(new.lines[ol.new_line], new))
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
    parts.append('.diff-intra-chg { border: solid 1px #cccc00; }')
    parts.append('.diff-intra-ins { border: solid 1px #00cc00; }')

    parts.append('.marked { background-color: #222255; }')

    parts.append('.mark-kept { color: #3366cc; }')
    parts.append('.mark-ins { color: #33cc66; }')
    parts.append('.mark-del { color: #cc3366; }')
    parts.append('.mark-chg { color: #cccc66; }')

    # Colors for light-background color schemes, like `friendly`
    #parts.append('.diff-old { background-color: #ffcccc; }')
    #parts.append('.diff-new { background-color: #ccffcc; }')
    #parts.append('.diff-intra-del { border: solid 1px #880000; }')
    #parts.append('.diff-intra-chg { border: solid 1px #888800; }')
    #parts.append('.diff-intra-ins { border: solid 1px #008800; }')

    parts.append(fmt.get_style_defs('.highlight'))
    return '\n'.join(parts) + '\n'
