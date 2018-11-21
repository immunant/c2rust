from collections import namedtuple
import pygments.formatters
import pygments.lexers
import pygments.token

from literate.annot import Span, fill_annot, cut_annot
from literate.file import File, Diff
import literate.diff
import literate.highlight


def render_line(line):
    parts = []
    def start_span(label, convert=lambda s: s):
        if label is not None:
            parts.append('<span class="%s">' % (convert(label),))
    def end_span(label):
        if label is not None:
            parts.append('</span>')


    if line.intra is not None:
        intra = fill_annot(line.intra, len(line.text))
    else:
        intra = [Span(0, len(line.text), None)]
    highlight = fill_annot(line.highlight, len(line.text))

    for (intra_span, sub_highlight) in cut_annot(highlight, intra):
        start_span(intra_span.label, lambda s: 'diff-intra-%s' % s)

        for highlight_span in sub_highlight:
            hl = highlight_span.label
            hl = None if hl in (pygments.token.Token, pygments.token.Text) else hl
            start_span(hl, lambda s: pygments.token.STANDARD_TYPES.get(s))

            start = highlight_span.start + intra_span.start
            end = highlight_span.end + intra_span.start

            parts.append(line.text[start : end])

            end_span(hl)

        end_span(intra_span.label)

    return ''.join(parts)

def make_file(path: str, text: str, nodes, marks_) -> File:
    f = File(path, text)
    literate.highlight.highlight_file(f)
    # TODO: marks
    return f

def make_diff(f1: File, f2: File) -> Diff:
    d = literate.diff.diff_files(f1, f2)
    from pprint import pprint
    pprint(d.blocks)
    literate.diff.build_diff_hunks(d)
    literate.diff.build_output_lines(d)
    return d

def render_diff(files, nodes, marks_):
    file_names = sorted(files.keys())
    empty = True

    parts = []
    parts.append('<table class="diff highlight">\n')
    parts.append('<colgroup>')
    parts.append('<col width="50"><col><col width="50"><col>')
    parts.append('</colgroup>\n')
    for f in file_names:
        old = make_file(f, files[f][0], nodes[f][0], marks_[0])
        new = make_file(f, files[f][1], nodes[f][1], marks_[1])
        diff = make_diff(old, new)

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
                    parts.append(render_line(old.lines[ol.old_line]))
                    parts.append('</pre></td>')
                else:
                    parts.append('<td></td><td></td>')

                if ol.new_line is not None:
                    parts.append('<td class="line-num %s">%d</td>' %
                            (new_cls, ol.new_line + 1))
                    parts.append('<td class="%s"><pre>' % new_cls)
                    parts.append(render_line(new.lines[ol.new_line]))
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

    # Colors for light-background color schemes, like `friendly`
    #parts.append('.diff-old { background-color: #ffcccc; }')
    #parts.append('.diff-new { background-color: #ccffcc; }')
    #parts.append('.diff-intra-del { border: solid 1px #880000; }')
    #parts.append('.diff-intra-chg { border: solid 1px #888800; }')
    #parts.append('.diff-intra-ins { border: solid 1px #008800; }')

    parts.append(fmt.get_style_defs('.highlight'))
    return '\n'.join(parts) + '\n'
