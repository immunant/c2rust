from typing import List, Tuple, Dict, Set, Optional, NamedTuple

from literate.annot import Span, Annot, fill_annot, zip_annot, cut_annot, \
        lookup_span, SpanMerger
from literate.file import File, Diff
from literate.points import Point, cut_points, annot_to_deltas


# This is the same info contained in a `marks.json` entry, just slightly more
# convenient to access.

class Mark(NamedTuple):
    '''This is the same info contained in a `marks.json` entry, just slightly
    more convenient to access.'''

    id: int
    '''The node ID of the marked node'''

    orig_id: int
    '''The "original node ID" reported by `c2rust-refactor`.  This is used to
    identify nodes across refactoring steps, so we can detect when a mark is
    left unchanged even if nodes got renumbered.'''

    labels: Set[str]
    '''A list of strings, giving the labels applied to the marked node.'''

    kind: str
    '''A string describing the kind of node that was marked.  `"item"`,
    `"expr"`, `"stmt"`, etc.'''

    name: Optional[str]
    '''The name of the marked node.  May be `None` or the empty string.  This
    is mainly useful on item nodes.'''

LabelChanges = Tuple[List[str], List[str], List[str]]

def convert_marks(marks: List[Dict]) -> Dict[int, Mark]:
    '''Convert a list of `marks.json` entries to `Mark` objects, and build a
    dict mapping each `Mark`'s `id` to the `Mark` itself.'''
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

CRATE_NODE_ID = 0

def build_mark_annot(f: File) -> Annot[Set[int]]:
    '''Build an annotation on the entire file, labeled with sets of NodeIds
    indicating the marked nodes overlapping each source location.'''
    # We start with one big annotation that labels the entire file with the
    # empty set (or the singleton set containing CRATE_NODE_ID, if the crate is
    # marked), and zip it with an annotation for each marked node in turn.
    if CRATE_NODE_ID not in f.marks:
        default = frozenset()
    else:
        default = frozenset((CRATE_NODE_ID,))
    annot = [Span(0, len(f.text), default)]

    for u_start, u_end, node_id in f.unformatted_nodes:
        if node_id not in f.marks:
            continue

        # `unformatted_nodes` uses source locations in the unformatted text,
        # which we need to translate to locations in the formatted text.
        start = f.fmt_map_translate(u_start)
        end = f.fmt_map_translate(u_end)

        node_annot = fill_annot([Span(start, end, frozenset((node_id,)))], 
                len(f.text), label=default)
        annot = zip_annot(annot, node_annot, f=lambda a, b: a | b)

    return annot


def init_line_mark_bounds(f: File):
    '''Initialize the `mark_starts` and `mark_ends` fields of the `Line`s in
    `f`.'''
    # We just build `mark_starts` and `mark_ends` point lists for the entire
    # file, and cut them up for the individual lines at the end.
    file_starts = []
    file_ends = []

    # Get a `point` for each change in `mark_annot`, indicating the sets of
    # overlapping marks before and after that point.
    for p in annot_to_deltas(f.mark_annot):
        old, new = p.label
        if old is None and new is None:
            continue
        elif old is None:
            started = new
            ended = set()
        elif new is None:
            started = set()
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
    '''Process marks for a file, initializing `File.marks`, `File.mark_annot`,
    `Line.mark_starts`, and `Line.mark_ends` fields.'''
    f.set_marks(convert_marks(f.raw_marks))

    # Annotate file with sets of marks
    annot = build_mark_annot(f)
    # Filter out spans labeled with the empty set.
    annot = [s for s in annot if len(s.label) > 0]
    f.set_mark_annot(annot)

    init_line_mark_bounds(f)


def init_hunk_start_marks(f: File, lines: Span):
    '''Set `Line.hunk_start_marks` on the first line in `lines` to the set of
    marked nodes that overlap its start.'''
    if lines.start >= len(f.lines):
        return
    line = lines.start
    char = f.line_annot[line].start
    # Figure out which marks cover the start position of the first line.  Only
    # marks that cross the line start count - ones that begin or end exactly at
    # line start are not included.
    mark_span = lookup_span(f.mark_annot, char,
            include_start=False, include_end=False)
    if mark_span is not None and len(mark_span.label) > 0:
        f.lines[line].set_hunk_start_marks(mark_span.label)

def init_hunk_end_marks(f: File, lines: Span):
    '''Set `Line.hunk_end_marks` on the last line in `lines` to the set of
    marked nodes that overlap its end.'''
    if lines.end <= 0:
        return
    line = lines.end - 1
    char = f.line_annot[line].end
    # Figure out which marks cover the end position of the last line.
    mark_span = lookup_span(f.mark_annot, char,
            include_start=False, include_end=False)
    if mark_span is not None and len(mark_span.label) > 0:
        f.lines[line].set_hunk_end_marks(mark_span.label)

def init_hunk_boundary_marks(d: Diff):
    '''Initialize `Line.hunk_start_marks` and/or `Line.hunk_end_marks` for
    lines at the start/end of each hunk in `d`.'''
    for h in d.hunks:
        changed, old_lines, new_lines = h.blocks[0]
        init_hunk_start_marks(d.old_file, old_lines)
        init_hunk_start_marks(d.new_file, new_lines)

        changed, old_lines, new_lines = h.blocks[-1]
        init_hunk_end_marks(d.old_file, old_lines)
        init_hunk_end_marks(d.new_file, new_lines)


def diff_labels(l1: Set[str], l2: Set[str]) -> LabelChanges:
    '''Diff two collections of labels, producing lists of labels added, labels
    removed, and labels kept.'''
    l1 = set(l1)
    l2 = set(l2)
    added = l2 - l1
    removed = l1 - l2
    kept = l1 & l2
    return (sorted(added), sorted(removed), sorted(kept))

def init_mark_labels(d: Diff):
    '''Diff the marks present in `d.old_files` and `d.new_files`, and use that
    to initialize `File.mark_labels` for both files.'''
    old_marks = dict((m.orig_id, m) for m in d.old_file.marks.values())
    new_marks = dict((m.orig_id, m) for m in d.new_file.marks.values())

    old_labels = {}
    for m in d.old_file.marks.values():
        if m.orig_id in new_marks:
            # There is a corresponding node marked in the new file.  Diff `m`'s
            # labels with the ones for that new node.
            old_labels[m.id] = diff_labels(m.labels, new_marks[m.orig_id].labels)
        else:
            # There is no corresponding node - the node (and its mark) must
            # have been deleted.
            old_labels[m.id] = ([], sorted(m.labels), [])
    d.old_file.set_mark_labels(old_labels)

    new_labels = {}
    for m in d.new_file.marks.values():
        if m.orig_id in old_marks:
            new_labels[m.id] = diff_labels(old_marks[m.orig_id].labels, m.labels)
        else:
            new_labels[m.id] = (sorted(m.labels), [], [])
    d.new_file.set_mark_labels(new_labels)


def init_file_keep_mark_lines(f: File):
    '''Initialize `f.keep_mark_lines` with an annotation covering the start of
    each node where a mark was added or removed.'''
    # Figure out which marks were changed - text for these will be kept in the
    # output even if it's not part of any hunk's context.
    keep_marks = set()
    for node_id, (added, removed, kept) in f.mark_labels.items():
        if len(added) > 0 or len(removed) > 0:
            keep_marks.add(node_id)

    # Get the start line for each kept mark.
    keep_start_lines = set()
    for u_start, u_end, node_id in f.unformatted_nodes:
        if node_id not in keep_marks:
            continue
        start = f.fmt_map_translate(u_start)
        line_span = lookup_span(f.line_annot, start)
        keep_start_lines.add(line_span.label)

    # Label a region around each mark's start line.
    keep_lines = SpanMerger()
    for start in sorted(keep_start_lines):
        keep_lines.add(Span(start - 3, start + 6))

    f.set_keep_mark_lines(keep_lines.finish())

def init_keep_mark_lines(d: Diff):
    '''Initialize `f.keep_mark_lines` for the old and new files of `d`.

    The two files are processed independently, but it relies on information
    derived from previous diff processing (namely, `File.mark_labels`).'''
    init_file_keep_mark_lines(d.old_file)
    init_file_keep_mark_lines(d.new_file)
