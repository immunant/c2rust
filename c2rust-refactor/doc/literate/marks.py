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


def diff_labels(l1, l2):
    l1 = set(l1)
    l2 = set(l2)
    added = l2 - l1
    removed = l1 - l2
    kept = l1 & l2
    return (sorted(added), sorted(removed), sorted(kept))

def init_mark_status(d: Diff):
    old_marks = dict((m.orig_id, m) for m in d.old_file.marks.values())
    new_marks = dict((m.orig_id, m) for m in d.new_file.marks.values())

    old_labels = {}
    for m in d.old_file.marks.values():
        if m.orig_id in new_marks:
            old_labels[m.id] = diff_labels(m.labels, new_marks[m.orig_id].labels)
        else:
            old_labels[m.id] = ((), sorted(m.labels), ())
    d.old_file.set_mark_labels(old_labels)

    new_labels = {}
    for m in d.new_file.marks.values():
        if m.orig_id in old_marks:
            new_labels[m.id] = diff_labels(old_marks[m.orig_id].labels, m.labels)
            print('label diff for %s (vs %s): %s' % (m.labels,
                old_marks[m.orig_id].labels, new_labels[m.id]))
        else:
            new_labels[m.id] = (sorted(m.labels), (), ())
    d.new_file.set_mark_labels(new_labels)


def init_file_keep_mark_lines(f: File):
    keep_marks = set()
    for node_id, (added, removed, kept) in f.mark_labels.items():
        if len(added) > 0 or len(removed) > 0:
            keep_marks.add(node_id)

    keep_start_lines = set()
    for u_start, u_end, node_id in f.unformatted_nodes:
        if node_id not in keep_marks:
            continue
        start = f.fmt_map_translate(u_start)
        line_span = lookup_span(f.line_annot, start)
        keep_start_lines.add(line_span.label)

    keep_lines = []
    def add(s):
        if len(keep_lines) > 0 and s.start in keep_lines[-1]:
            keep_lines[-1].end = max(keep_lines[-1].end, s.end)
        else:
            keep_lines.append(s)

    for start in sorted(keep_start_lines):
        add(Span(start - 3, start + 6))

    f.set_keep_mark_lines(keep_lines)

def init_keep_mark_lines(d: Diff):
    init_file_keep_mark_lines(d.old_file)
    init_file_keep_mark_lines(d.new_file)
