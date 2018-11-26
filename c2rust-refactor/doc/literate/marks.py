from collections import namedtuple

from literate.annot import Span, fill_annot, zip_annot, cut_annot, lookup_span
from literate.file import File, Diff
from literate.points import Point, cut_points, annot_to_deltas


Mark = namedtuple('Mark', ('id', 'orig_id', 'labels', 'kind', 'name'))

def convert_marks(marks: [dict]) -> {int: Mark}:
    result = {}
    for m in marks:
        if len(m['labels']) == 0:
            continue
        assert m['id'] not in result
        result[m['id']] = Mark(
                m['id'],
                m['orig_id'],
                m['labels'],
                m['kind'],
                m['name'],
                )
    return result

def marks_annot(f: File) -> [Span]:
    '''Annotate the entire file text with sets of NodeIds, indicating the
    marked nodes overlapping each location.'''
    empty = frozenset()
    annot = [Span(0, len(f.text), empty)]

    for u_start, u_end, node_id in f.unformatted_nodes:
        if node_id not in f.marks:
            continue

        start = f.fmt_map_translate(u_start)
        end = f.fmt_map_translate(u_end)

        print('mapped span %d..%d (%r) to %d..%d (%r)' %
                (u_start, u_end, f.unformatted[u_start : u_end],
                    start, end, f.text[start : end]))

        node_annot = fill_annot([Span(start, end, frozenset((node_id,)))], 
                len(f.text), label=empty)
        annot = zip_annot(annot, node_annot, f=lambda a, b: a | b)

    return annot

# For mark bounds, we'd really like to label *positions*, not spans.  So we use
# a bit of a hack: we store the original span's start and end positions in the
# label, and compare them to the start and end positions during rendering.
# This lets us detect if we are actually looking at a cut-up subspan of the
# original, in which case the started/ended sets may not actually apply to the
# cut span's endpoints.
MarkBoundsLabel = namedtuple('MarkBoundsLabel',
        ('marks_started', 'orig_start', 'marks_ended', 'orig_end'))

def init_line_mark_bounds(f: File):
    file_starts = []
    file_ends = []

    for p in annot_to_deltas(f.mark_annot):
        old, new = p.label
        if old is None and new is None:
            continue
        elif old is None:
            started = new
            ended = ()
        elif new is None:
            started = ()
            ended = old
        else:
            started = new - old
            ended = old - new

        if len(started) > 0:
            file_starts.append(Point(p.pos, started))
        if len(ended) > 0:
            file_ends.append(Point(p.pos, ended))

    for line_span, line_starts in cut_points(file_starts, f.line_annot,
            include_start=True, include_end=False):
        f.lines[line_span.label].set_mark_starts(line_starts)

    for line_span, line_ends in cut_points(file_ends, f.line_annot,
            include_start=False, include_end=True):
        f.lines[line_span.label].set_mark_ends(line_ends)

def mark_file(f: File):
    from pprint import pprint
    f.set_marks(convert_marks(f.raw_marks))

    # Annotate file with sets of marks
    annot = marks_annot(f)
    annot = [s for s in annot if len(s.label) > 0]
    f.set_mark_annot(annot)

    init_line_mark_bounds(f)


def init_hunk_start_marks(f: File, lines: Span):
    if lines.start >= len(f.lines):
        return
    line = lines.start
    char = f.line_annot[line].start
    mark_span = lookup_span(f.mark_annot, char,
            include_start=True, include_end=False)
    if mark_span is not None and len(mark_span.label) > 0:
        f.lines[line].set_hunk_start_marks(mark_span.label)

def init_hunk_end_marks(f: File, lines: Span):
    if lines.end <= 0:
        return
    line = lines.end - 1
    char = f.line_annot[line].end
    mark_span = lookup_span(f.mark_annot, char,
            include_start=False, include_end=True)
    if mark_span is not None and len(mark_span.label) > 0:
        f.lines[line].set_hunk_end_marks(mark_span.label)

def init_hunk_boundary_marks(d: Diff):
    for h in d.hunks:
        changed, old_lines, new_lines = h.blocks[0]
        init_hunk_start_marks(d.old_file, old_lines)
        init_hunk_start_marks(d.new_file, new_lines)

        changed, old_lines, new_lines = h.blocks[-1]
        init_hunk_end_marks(d.old_file, old_lines)
        init_hunk_end_marks(d.new_file, new_lines)
