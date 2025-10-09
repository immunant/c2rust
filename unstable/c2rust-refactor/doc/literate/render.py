from collections import namedtuple
import html
import re
from typing import Dict, Optional, Any

import pygments.formatters

from literate.annot import Span, fill_annot, cut_annot
from literate.file import File, Line, Diff
from literate.points import Point, map_points, merge_points, \
        annot_starts, annot_ends, cut_annot_at_points
import literate.diff
import literate.highlight
import literate.marks


def mark_class(f: File, node_id: int) -> str:
    '''Get the CSS class suffix to use for `node_id`'s mark indicator.'''
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
    '''Get a user-facing description of the mark on `node_id`.

    The returned descriptions look like:

        item #123: added 'foo'; removed 'bar', 'baz'; kept 'target'
    '''
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

def render_line(line: Line, f: File, opts: Dict[str, Any]) -> str:
    '''Render HTML output for a single line of a file.  `f` should be the file
    containing `line`.'''
    parts = []

    # Helpers for adding common types of parts, such as span start/end tags.

    def mark_arrow(m, start):
        if start:
            sym = '&#x25b6;'
        else:
            sym = '&#x25c0;'
        parts.append('<span class="mark-%s" title="%s">%s</span>' %
                (mark_class(f, m), html.escape(mark_desc(f, m)), sym))

    def emit_text(start, end):
        if end == len(line.text) and line.text.endswith('\n'):
            # Avoid emitting the line's trailing `\n`.  It normally gets
            # ignored by the browser, but sometimes we emit a mark indicator
            # afterward, which we'd like to stay on the same line when
            # possible.
            end = end - 1
        if end > start:
            parts.append(line.text[start : end])

    def start_span(cls):
        parts.append('<span class="%s">' % cls)

    def end_span():
        parts.append('</span>')


    # Build a list of "events", which are places where we should insert markup
    # tags into the text of the line.  `events` is a list of `Point`s, each
    # labeled with details of the tag to be inserted at the point.

    # We rely on the bias of `merge_points` (when two input sequences have
    # points at the same position, the ones from the first input appear first)
    # to establish an ordering between different event types and ensure proper
    # nesting of HTML tags.
    #
    # The most interesting case here is for `line.intra`.  The CSS for
    # intraline edit markers uses a `border` attribute, which means a pair of
    # adjacent intraline edit spans is visibly distinct from a single longer
    # span.  (This is unlike `highlight`, whose CSS only sets text color.)  We
    # deliberately break intraline edit spans around mark indicators, but
    # otherwise try to keep them intact.

    events = []

    # Mark starts and ends.
    events = merge_points(
            map_points(line.mark_ends, lambda l: ('m_e', l)),
            events,
            map_points(line.mark_starts, lambda l: ('m_s', l)),
            )

    # Intraline edits.
    if line.intra is not None and opts['diff-style'] != 'only-new':
        intra = cut_annot_at_points(line.intra, events)
        events = merge_points(
                map_points(annot_ends(intra), lambda l: ('i_e', l)),
                events,
                map_points(annot_starts(intra), lambda l: ('i_s', l)),
                )

    # Syntax highlighting.
    highlight = cut_annot_at_points(line.highlight, events)
    events = merge_points(
            map_points(annot_ends(highlight), lambda l: ('hl_e', l)),
            events,
            map_points(annot_starts(highlight), lambda l: ('hl_s', l)),
            )


    # Build up the output text.  We emit special tags at each point in
    # `events`, and otherwise emit substrings from `line.text`.

    last_pos = 0

    if line.hunk_start_marks:
        # We sort all starting labels descending and all ending labels
        # ascending, in hopes that higher-numbered labels enclose
        # lower-numbered ones.  There's no guarantee, of course.
        for m in sorted(line.hunk_start_marks, reverse=True):
            mark_arrow(m, True)

            # These hunk-start mark indicators often appear before the
            # whitespace of indented lines.  To avoid throwing off the
            # indentation, we let them "eat up" as much whitespace as is
            # available.  Any start/end events in the eaten whitespace will be
            # pushed back until after the marks.  (Hopefully there are not many
            # events inside a purely whitespace span.)
            if last_pos < len(line.text) and line.text[last_pos] == ' ':
                last_pos += 1

    hl_mode = opts['highlight-mode']

    for p in events:
        if p.pos > last_pos:
            emit_text(last_pos, p.pos)
        last_pos = p.pos

        kind, label = p.label

        if kind in 'm_s':
            for m in sorted(label, reverse=True):
                mark_arrow(m, True)
        elif kind == 'm_e':
            for m in sorted(label):
                mark_arrow(m, False)
        elif kind == 'i_s':
            start_span('diff-intra-%s' % label)
        elif kind == 'i_e':
            end_span()
        elif kind == 'hl_s':
            cls = literate.highlight.token_css_class(label, hl_mode)
            if cls is not None:
                start_span(cls)
        elif kind == 'hl_e':
            cls = literate.highlight.token_css_class(label, hl_mode)
            if cls is not None:
                end_span()

    if len(line.text) > last_pos:
        emit_text(last_pos, len(line.text))

    if line.hunk_end_marks:
        for m in sorted(line.hunk_end_marks):
            mark_arrow(m, False)

    return ''.join(parts)

def annotate_irrelevant(f: File, start: str, end: str):
    start_re = re.compile(start)
    end_re = re.compile(end)

    result = []
    start_line = None
    for i, l in enumerate(f.lines):
        if start_line is None and start_re.match(l.text):
            start_line = i
        if start_line is not None and end_re.match(l.text):
            result.append(Span(start_line, i + 1, None))
            start_line = None

    f.set_drop_irrelevant_lines(result)

def prepare_files(files: [File]):
    '''Run single-file initialization steps on each file in `files`.'''
    for i, f in enumerate(files):
        print('preparing file %d (%s)' % (i, f.path))
        literate.highlight.highlight_file(f)
        literate.marks.mark_file(f)

def make_diff(f1: File, f2: File, opts: Dict[str, Any]) -> Diff:
    '''Construct a diff between two files, and run diff initialization
    steps.'''
    print('  diffing file %s' % f1.path)

    f1 = f1.copy()
    f2 = f2.copy()

    # These options get handled during diff construction because they can vary
    # across refactoring blocks, while each `File` in `prepare_files` is
    # typically used in 2 different blocks.
    if opts['irrelevant-start-regex'] and opts['irrelevant-end-regex']:
        start = opts['irrelevant-start-regex']
        end = opts['irrelevant-end-regex']
        annotate_irrelevant(f1, start, end)
        annotate_irrelevant(f2, start, end)

    d = literate.diff.diff_files(f1, f2)
    literate.marks.init_mark_labels(d)
    literate.marks.init_keep_mark_lines(d)
    literate.diff.build_diff_hunks(d, opts['diff-style'] == 'context')
    literate.diff.build_output_lines(d)
    literate.marks.init_hunk_boundary_marks(d)

    return d

def render_diff(old_files: Dict[str, File], new_files: Dict[str, File],
        opts: Dict[str, Any]) -> Optional[str]:
    '''Render a diff between each file in `new_files` and the corresponding one
    in `old_files`.  The result is either a string of HTML source, or `None` if
    nothing changed.'''

    if opts['hide-diff']:
        return None

    omit_old = opts['diff-style'] == 'only-new'
    cols = 4 if not omit_old else 2

    file_names = sorted(new_files.keys())
    # Is the diff empty?
    empty = True

    parts = []
    parts.append('<table class="diff %s">\n' %
            literate.highlight.get_highlight_class(opts))
    parts.append('<colgroup>')
    if not omit_old:
        parts.append('<col width="50"><col>')
    parts.append('<col width="50"><col>')
    parts.append('</colgroup>\n')
    for file_idx, f in enumerate(file_names):
        # `make_diff` copies the files, then updates the copies.  We want
        # references to the new copies only.
        diff = make_diff(old_files[f], new_files[f], opts)
        old = diff.old_file
        new = diff.new_file

        if len(diff.hunks) > 0:
            empty = False

        if opts['show-filename']:
            parts.append('<tr><td colspan="%d" class="filename">%s</td></tr>' % (cols, f))
        else:
            if file_idx > 0:
                parts.append('<tr><td colspan="%d"><hr></td></tr>' % cols)

        first = True
        for hunk in diff.hunks:
            if not first:
                parts.append('<tr><td colspan="%d"><hr></td></tr>' % cols)
            first = False

            for ol in hunk.output_lines:
                old_cls = 'diff-old' if ol.changed else ''
                new_cls = 'diff-new' if ol.changed and not omit_old else ''

                parts.append('<tr>')

                if not omit_old:
                    if ol.old_line is not None:
                        parts.append('<td class="line-num %s">%d</td>' %
                                (old_cls, ol.old_line + 1))
                        parts.append('<td class="%s"><pre>' % old_cls)
                        parts.append(render_line(old.lines[ol.old_line], old, opts))
                        parts.append('</pre></td>')
                    else:
                        parts.append('<td></td><td></td>')

                if ol.new_line is not None:
                    parts.append('<td class="line-num %s">%d</td>' %
                            (new_cls, ol.new_line + 1))
                    parts.append('<td class="%s"><pre>' % new_cls)
                    parts.append(render_line(new.lines[ol.new_line], new, opts))
                    parts.append('</pre></td>')
                else:
                    parts.append('<td></td><td></td>')

                parts.append('</tr>\n')

    parts.append('</table>\n')

    if empty:
        return None

    return ''.join(parts)


def get_styles() -> str:
    '''Return CSS styles for displaying generated diffs.'''
    parts = []
    parts.append('table.highlight { width: 800px; table-layout: fixed; }')
    parts.append('.highlight td { overflow-wrap: break-word; }')
    parts.append('.highlight td.line-num { text-align: right; }')
    parts.append('.highlight pre { display: inline; white-space: pre-wrap; }')

    # Diff highlight colors for use with `monokai` color scheme
    parts.append('.diff-old { background-color: rgba(255, 0, 0, 0.25); }')
    parts.append('.diff-new { background-color: rgba(0, 255, 0, 0.20); }')
    parts.append('.diff-intra-del { border: solid 1px #cc0000; }')
    parts.append('.diff-intra-chg { border: solid 1px #cccc00; }')
    parts.append('.diff-intra-ins { border: solid 1px #00cc00; }')

    # Monokai colors for mark indicators (taken from pygments.styles.monokai)
    parts.append('.mark-kept { color: #66d9ef; border: solid 1px; }')
    parts.append('.mark-ins { color: #a6e22e; border: solid 1px; }')
    parts.append('.mark-del { color: #f92672; border: solid 1px; }')
    parts.append('.mark-chg { color: #e6db74; border: solid 1px; }')

    # Colors for light-background color schemes, like `friendly`
    #parts.append('.diff-old { background-color: #ffcccc; }')
    #parts.append('.diff-new { background-color: #ccffcc; }')
    #parts.append('.diff-intra-del { border: solid 1px #880000; }')
    #parts.append('.diff-intra-chg { border: solid 1px #888800; }')
    #parts.append('.diff-intra-ins { border: solid 1px #008800; }')

    # Compatibility with manual/mdbook rendering
    parts.append('.diff { font-size: 0.875em; }')
    parts.append('.diff td { padding: 2px; }')
    parts.append('.diff td pre { margin: 0; }')
    parts.append('table.diff tbody tr:nth-child(2n) { background: inherit; }')

    return '\n'.join(parts) + '\n'

def get_pygments_styles(fmt=None) -> str:
    fmt = fmt or pygments.formatters.get_formatter_by_name(
            'html', nowrap=True, style='monokai')

    return fmt.get_style_defs('.highlight')
