from collections import namedtuple

from literate.annot import Span, fill_annot, zip_annot, cut_annot
from literate.file import File
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
