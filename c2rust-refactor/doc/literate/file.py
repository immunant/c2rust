from collections import namedtuple
import literate.annot

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
        # Marks.  Annotation data is a set of NodeIds.
        self.marks = None

    def copy(self):
        c = Line(self.text)
        c.highlight = self.highlight
        c.intra = self.intra
        c.marks = self.marks
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


class File:
    '''Tracks data about a single file in isolation.'''
    def __init__(self, path, text):
        self.path = path
        self.raw = text
        self.lines = [Line(l) for l in text.splitlines(keepends=True)]
        self.line_text = [l.text for l in self.lines]
        # A list of tuples (node ID, source span).  This isn't a map because
        # the same node ID can appear with multiple spans, if the original AST
        # node was duplicated during refactoring.
        self.nodes = None
        # An annotation of `raw`, where the annotation data is the line number
        # (0-based).  Useful for intersecting/cutting other `raw` annotations
        # to get individual line annotations.
        self.line_annot = literate.annot.number_lines(self.line_text)

    def copy(self):
        c = File(self.path, '')
        c.raw = self.raw
        c.lines = [l.copy() for l in self.lines]
        c.nodes = self.nodes
        return c

    def set_nodes(self, nodes):
        assert self.nodes is None
        self.nodes = nodes


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
