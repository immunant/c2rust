import bisect
from typing import List, Tuple, Dict, Set, Optional, NamedTuple

import pygments.token

import literate.annot
from literate.annot import Span, Annot
from literate.points import Point


# The general pattern here is: all fields start as `None`, and get initialized
# at most once.  The value is never changed after being set.  The exceptions
# are `File.lines` and `Diff.old/new_file`, where the referenced object may
# have additional fields set after the parent object is created.


class Line:
    '''A line is a string, which may have various kinds of annotations applied
    to its substrings.'''

    text: str
    '''The text of the line, including trailing `\n`.'''

    # Extra spaces make it easier to see the important part.  All these fields
    # are `Optional` because they start as `None` before `set_foo` is called.

    highlight: Optional[ Annot[type(pygments.token.Token)] ]
    '''Syntax highlighting.  Annotation data is a pygments token.'''

    intra: Optional[ Annot[str] ]
    '''Intraline edits.  Annotation data is one of the strings `ins`, `del`, or
    `chg`, indicating that the annotated substring was inserted, deleted, or
    changed by an intraline edit.'''

    marks: Optional[ Set[int] ]
    '''Annotates each `text` position with a set of node IDs, indicating nodes
    that overlap the position and have at least one mark.'''

    mark_starts: Optional[ List[Point[Set[int]]] ]
    '''Points where marked nodes begin.  Point labels are sets of node IDs, as
    in `marks`.  The begin and end parts are kept separate so they can be
    handled at different points in `render.render_line`'s event ordering.'''
    mark_ends: Optional[ List[Point[Set[int]]] ]
    '''Points where marked nodes end.  See `mark_starts` for details.'''

    hunk_start_marks: Optional[ Set[int] ]
    '''Set of node IDs for marks that are present at the start of the line,
    when this line is the start of a hunk.  This field remains `None` for lines
    not at the start of a hunk, or where there are no marks that cross the hunk
    boundary.'''
    hunk_end_marks: Optional[ Set[int] ]
    '''Set of node IDs for marks that are present at the end of the line,
    when this line is the end of a hunk.  See `hunk_start_marks` for
    details.'''

    def __init__(self, text: str):
        self.text = text
        self.highlight = None
        self.intra = None
        self.marks = None
        self.mark_starts = None
        self.mark_ends = None
        self.hunk_start_marks = None
        self.hunk_end_marks = None

    def copy(self) -> 'Line':
        c = Line(self.text)
        # Shallow copy is fine for all fields, since the values are expected to
        # be immutable once the field is initialized.
        c.highlight = self.highlight
        c.intra = self.intra
        c.marks = self.marks
        c.mark_starts = self.mark_starts
        c.mark_ends = self.mark_ends
        c.hunk_start_marks = self.hunk_start_marks
        c.hunk_end_marks = self.hunk_end_marks
        return c

    def set_highlight(self, highlight: Annot[type(pygments.token.Token)]):
        assert self.highlight is None
        self.highlight = highlight

    def set_intra(self, intra: Annot[str]):
        assert self.intra is None
        self.intra = intra

    def set_marks(self, marks: Annot[Set[int]]):
        assert self.marks is None
        self.marks = marks

    def set_mark_starts(self, mark_starts: List[Point[Set[int]]]):
        assert self.mark_starts is None
        self.mark_starts = mark_starts

    def set_mark_ends(self, mark_ends: List[Point[Set[int]]]):
        assert self.mark_ends is None
        self.mark_ends = mark_ends

    def set_hunk_start_marks(self, hunk_start_marks: Set[int]):
        assert self.hunk_start_marks is None
        self.hunk_start_marks = hunk_start_marks

    def set_hunk_end_marks(self, hunk_end_marks: Set[int]):
        assert self.hunk_end_marks is None
        self.hunk_end_marks = hunk_end_marks


class File:
    '''Everything we know about a single file's contents.  This starts out
    mostly uninitialized, and gets filled in gradually as we run various
    processing and diff construction steps.'''

    path: str
    '''The path to the file.  There's no set format (absolute vs relative) - we
    just use whatever rustc spits out, which should always be valid relative to
    the directory where refactoring ran.'''

    unformatted: str
    '''The full text of the file, before `rustfmt`.  This is the exact input or
    output of some step in the refactoring process.'''

    raw_marks: List[Dict]
    '''Raw mark data, as loaded from `marks.N.json`.  These marks actually
    cover the entire crate, not a particular file.  That means they're the same
    for every file that exists at the same stage of refactoring, and also they
    may refer to spans that fall outside this file.'''

    marks: Optional[ Dict[int, 'literate.marks.Mark'] ]
    '''Processed marks, indexed by node ID.'''

    mark_annot: Optional[ Annot[Set[int]] ]
    '''Annotates each `text` position with a set of node IDs, indicating nodes
    that overlap the position and have at least one mark.'''

    mark_labels: Optional[ Dict[int, 'literate.marks.LabelChanges'] ]
    '''Maps each marked node ID to info on labels inserted, deleted, or kept
    intact on that node.'''

    keep_mark_lines: Optional[ Annot[None] ]
    '''Annotates lines that should be kept in the diff (regardless of proximity
    to textual changes) due to marks being inserted/deleted nearby.'''

    text: Optional[ str ]
    '''The formatted text of the file.'''

    lines: Optional[ List[Line] ]
    '''The lines of the file, represented as annotated `Line`s.  Unlike most
    other fields, it's okay to mutate the `Line`s in this list after this field
    is initialized.'''

    line_text: Optional[ List[str] ]
    '''The lines of the file, as plain text.  This is just `[l.text for l in
    self.lines]`.'''

    line_annot: Optional[ Annot[int] ]
    '''An annotation of the formatted `text`, labeling the text of each line
    with its (0-based) index in `lines`.'''

    fmt_map: Optional[ List[Tuple[Span[None], int]] ]
    '''Maps unformatted to formatted text positions.  The `Span` covers a
    section of unformatted text that was passed through formatting unchanged,
    and the `int` gives the start position of that text in the formatted
    version.  This field (along with `fmt_map_index`) is automatically
    populated on-demand, as it's somewhat expensive to compute and is only
    needed when `marks` is non-empty.'''
    fmt_map_index: Optional[ List[int] ]
    '''The start position of the `Span` part of each pair in `fmt_map`.  This
    is used for lookups with `bisect`.'''

    def __init__(self,
            path: str,
            text: str,
            nodes: List[Tuple[int, int, int]],
            marks: List[Dict]):
        self.path = path

        self.unformatted = text
        self.unformatted_nodes = nodes
        self.raw_marks = marks

        self.marks = None
        self.mark_annot = None
        self.mark_labels = None
        self.keep_mark_lines = None

        self.text = None
        self.lines = None
        self.line_text = None
        self.line_annot = None

        self.fmt_map = None
        self.fmt_map_index = None

    def copy(self) -> 'File':
        c = File(self.path,
                self.unformatted, self.unformatted_nodes, self.raw_marks)
        # Shallow copy is fine for everything except `lines`, which can have
        # additional fields initialized at any time.  Thanks to this, the
        # copied file is totally independent of the original (assuming values
        # in initialized fields are never mutated).
        c.marks = self.marks
        c.mark_annot = self.mark_annot
        c.mark_labels = self.mark_labels
        c.keep_mark_lines = self.keep_mark_lines
        c.text = self.text
        c.fmt_map = self.fmt_map
        c.fmt_map_index = self.fmt_map_index
        c.lines = [l.copy() for l in self.lines]
        c.line_text = self.line_text
        c.line_annot = self.line_annot
        return c

    def set_formatted(self, text: str):
        '''Provide formatted text for this file.  This also initializes other
        fields that are derived from the formatted text, particularly
        `lines`.'''
        assert self.text is None
        self.text = text
        self.lines = [Line(l) for l in text.splitlines(keepends=True)]
        self.line_text = [l.text for l in self.lines]
        self.line_annot = literate.annot.number_lines(self.line_text)

    def set_marks(self, marks: 'Dict[int, literate.marks.Mark]'):
        assert self.marks is None
        self.marks = marks

    def set_mark_annot(self, mark_annot: Annot[Set[int]]):
        assert self.mark_annot is None
        self.mark_annot = mark_annot

    def set_mark_labels(self,
            mark_labels: Dict[int, 'literate.marks.LabelChanges']):
        assert self.mark_labels is None
        self.mark_labels = mark_labels

    def set_keep_mark_lines(self, keep_mark_lines: Annot[None]):
        assert self.keep_mark_lines is None
        self.keep_mark_lines = keep_mark_lines

    def set_fmt_map(self, fmt_map: List[Tuple[Span[None], int]],
            fmt_map_index: List[int]):
        assert self.fmt_map is None
        self.fmt_map = fmt_map
        assert self.fmt_map_index is None
        self.fmt_map_index = fmt_map_index

    def _init_fmt_map(self):
        import literate.format
        literate.format.init_fmt_map(self)

    def fmt_map_lookup(self, unformatted_pos: int) -> Tuple[Span[None], int]:
        '''Look up an unformatted text position, returning a (span, offset)
        pair.  `span` is the containing span in the unformatted text (or a
        nearby span, if `unformatted_pos` is in text that was modified by
        formatting), and `offset` is the offset corresponding to `span.start`
        in the formatted text.'''
        if self.fmt_map is None:
            self._init_fmt_map()

        i = bisect.bisect_right(self.fmt_map_index, unformatted_pos)
        if i == 0:
            # Dummy result
            return (Span(0, 0), 0)
        else:
            return self.fmt_map[i - 1]

    def fmt_map_translate(self, unformatted_pos: int) -> int:
        '''Translate an unformatted text position to a corresponding position
        in the formatted text.'''
        # TODO: When `unformatted_pos` lies inside of text that was deleted
        # during formatting, this code probably produces bad results.  But only
        # whitespace and the occasional punctuation should ever get deleted by
        # rustfmt, and those usually do not contain the endpoints of marked
        # nodes, which is the main use case for this function.
        span, new_start = self.fmt_map_lookup(unformatted_pos)
        delta = unformatted_pos - span.start
        if delta > len(span):
            delta = len(span)
        return new_start + delta


DiffBlock = Tuple[bool, Span[None], Span[None]]

class Diff:
    '''Maps related lines between old and new files.  Note that this class does
    *not* include intraline diff info - that is exposed as annotations on the
    `File`s' `Line`s.'''

    old_file: File
    new_file: File

    blocks: List[DiffBlock]
    '''A list of tuples (changed, old line span, new line span).  The spans
    cover all lines of the old and new files, in order, with no gaps.'''

    hunks: Optional[ List['Hunk'] ]
    '''A list of diff hunks, formatted for output as a two-column diff.'''

    def __init__(self, old_file: File, new_file: File, blocks: List[DiffBlock]):
        self.old_file = old_file
        self.new_file = new_file
        self.blocks = blocks
        self.hunks = None

    def set_hunks(self, hunks: List['Hunk']):
        assert self.hunks is None
        self.hunks = hunks

class OutputLine(NamedTuple):
    '''A line of the two-column output.  `changed` is a boolean indicating
    whether this line is an insertion/deletion/change or context.  `old_line`
    and `new_line` are the indexes of lines to display from the old/new file,
    and can be `None` in cases of unbalanced insertions/deletions.'''
    changed: bool
    old_line: Optional[int]
    new_line: Optional[int]

class Hunk:
    '''A single diff hunk for output.'''

    blocks: List[DiffBlock]
    '''Formatted identically to `Diff.blocks`, but it may not cover the
    entirety of the old and new files.  (It's still contiguous, though.)'''

    output_lines: Optional[ List[OutputLine] ]
    '''Two-column output lines, ready for rendering (in combination with the
    old and new files).'''

    def __init__(self, blocks: List[DiffBlock]):
        self.blocks = blocks
        self.output_lines = None

    def set_output_lines(self, output_lines: List[OutputLine]):
        assert self.output_lines is None
        self.output_lines = output_lines
