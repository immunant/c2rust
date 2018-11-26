import bisect
from collections import namedtuple

import literate.annot
from literate.annot import Span


# The general pattern here is: all fields start as `None`, and get initialized
# at most once.  The value is never changed after being set.  The exceptions
# are `File.lines` and `Diff.old/new_file`, where the referenced object may
# have additional fields set after the parent object is created.


class Line:
    '''A line is a string, which may have various kinds of annotations applied
    to its substrings.'''
    def __init__(self, text):
        self.text = text
        # Syntax highlighting.  Annotation data is a pygments token.
        self.highlight = None
        # Intraline edits.  Annotation data is `True`, indicating that the
        # annotated substring was inserted/deleted by an intraline edit.
        self.intra = None
        # Marked node text.  Annotation data is a set of NodeIds, indicating
        # which of the nodes overlapping the position have at least one mark.
        self.marks = None
        # Points where marks begin or end.  Each is a list of points, labeled
        # with a set of node IDs.
        self.mark_starts = None
        self.mark_ends = None
        # Sets of node IDs for marks that are present at the start of the line
        # (when it's the start of a hunk) or the end of a line (when it's the
        # end of a hunk).  These remain `None` for lines not at the start/end
        # of a hunk, or where there are no marks covering a hunk boundary.
        self.hunk_start_marks = None
        self.hunk_end_marks = None

    def copy(self):
        c = Line(self.text)
        c.highlight = self.highlight
        c.intra = self.intra
        c.marks = self.marks
        c.mark_starts = self.mark_starts
        c.mark_ends = self.mark_ends
        c.hunk_start_marks = self.hunk_start_marks
        c.hunk_end_marks = self.hunk_end_marks
        return c

    def set_highlight(self, highlight):
        assert self.highlight is None
        self.highlight = highlight

    def set_intra(self, intra):
        assert self.intra is None
        self.intra = intra

    def set_marks(self, marks):
        assert self.marks is None
        self.marks = marks

    def set_mark_starts(self, mark_starts):
        assert self.mark_starts is None
        self.mark_starts = mark_starts

    def set_mark_ends(self, mark_ends):
        assert self.mark_ends is None
        self.mark_ends = mark_ends

    def set_hunk_start_marks(self, hunk_start_marks):
        assert self.hunk_start_marks is None
        self.hunk_start_marks = hunk_start_marks

    def set_hunk_end_marks(self, hunk_end_marks):
        assert self.hunk_end_marks is None
        self.hunk_end_marks = hunk_end_marks


class File:
    '''Everything we know about a single file's contents.  This starts out
    mostly uninitialized, and gets filled in gradually as we run various
    processing and diff construction steps.'''
    def __init__(self, path, text, nodes, marks):
        self.path = path

        # The text as it appeared before formatting.
        self.unformatted = text

        # A list of nodes present in the file.  Each entry is a tuple of
        # (start, end, node ID).  Note that a single node ID can appear
        # multiple times when a node is duplicated.
        self.unformatted_nodes = nodes

        # The raw marks, as loaded from JSON.  These actually cover the entire
        # crate, so they are the same for every `File` in a `CrateState`, and
        # may refer to nodes that aren't present in `unformatted_nodes`.
        self.raw_marks = marks

        # Processed marks - a dict mapping node IDs to `literate.marks.Mark`
        # objects.
        self.marks = None

        # An annotation of `text`, indicating the marked node(s) containing
        # each point in the file.
        self.mark_annot = None

        # The formatted text of the file.
        self.text = None

        # Info about the insertions/deletions performed during formatting.
        # This is constructed on demand.  It's used to map the span endpoints
        # from `raw_nodes`, which are always relative to the `unformatted`
        # text, to positions relative to `text`.
        self.fmt_map = None
        self.fmt_map_index = None

        # These all get filled in once the formatted text is available.

        # The lines of the file, as `Line`s.
        self.lines = None
        # The lines of the file, as plain `str`s (text only, no annotations).
        self.line_text = None
        # An annotation of `text`, where the annotation data is the line number
        # (0-based).  Useful for intersecting/cutting other `text` annotations
        # to get individual line annotations.
        self.line_annot = None

    def copy(self):
        c = File(self.path,
                self.unformatted, self.unformatted_nodes, self.raw_marks)
        c.marks = self.marks
        c.mark_annot = self.mark_annot
        c.text = self.text
        c.fmt_map = self.fmt_map
        c.fmt_map_index = self.fmt_map_index
        c.lines = [l.copy() for l in self.lines]
        c.line_text = self.line_text
        c.line_annot = self.line_annot
        return c

    def set_formatted(self, text):
        assert self.text is None
        self.text = text
        self.lines = [Line(l) for l in text.splitlines(keepends=True)]
        self.line_text = [l.text for l in self.lines]
        self.line_annot = literate.annot.number_lines(self.line_text)

    def set_marks(self, marks):
        assert self.marks is None
        self.marks = marks

    def set_mark_annot(self, mark_annot):
        assert self.mark_annot is None
        self.mark_annot = mark_annot

    def set_fmt_map(self, fmt_map, fmt_map_index):
        assert self.fmt_map is None
        self.fmt_map = fmt_map
        assert self.fmt_map_index is None
        self.fmt_map_index = fmt_map_index

    def _init_fmt_map(self):
        import literate.format
        literate.format.init_fmt_map(self)

    def fmt_map_lookup(self, unformatted_pos: int) -> (Span, int):
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
        span, new_start = self.fmt_map_lookup(unformatted_pos)
        delta = unformatted_pos - span.start
        print('translate: %d -> %s, %d -> delta %d -> %d' %
                (unformatted_pos, span, new_start, delta, new_start +
                    min(delta, len(span))))
        if delta > len(span):
            delta = len(span)
        return new_start + delta

class Diff:
    '''Maps related lines between old and new files.  Note that this class does
    *not* include intraline diff info - that is exposed as line annotations on
    the `File`.'''
    def __init__(self, old_file, new_file, blocks):
        self.old_file = old_file
        self.new_file = new_file
        # A list of tuples (changed, old line span, new line span).  The spans
        # cover all lines of the old and new files, in order, with no gaps.
        self.blocks = blocks
        # A list of hunks, formatted for output as a two-column diff.
        self.hunks = None

    def set_hunks(self, hunks):
        assert self.hunks is None
        self.hunks = hunks

class Hunk:
    '''A single diff hunk for output.'''
    def __init__(self, blocks):
        # Formatted identically to `Diff.blocks`, but it may not cover the
        # entirety of the old and new files.
        self.blocks = blocks
        # A list of output lines in the hunk.
        self.output_lines = None

    def set_output_lines(self, output_lines):
        assert self.output_lines is None
        self.output_lines = output_lines

# A line of the two-column output.  `changed` is a boolean indicating whether
# this line is an insertion/deletion/change or context.  `old_line` and
# `new_line` are the lines to display from the old/new file, and can be `None`
# for unbalanced insertions/deletions.
OutputLine = namedtuple('OutputLine', ('changed', 'old_line', 'new_line'))
