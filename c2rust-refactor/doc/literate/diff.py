from collections import namedtuple, deque
import difflib
import pygments.formatters
import pygments.lexers
import pygments.token
import re


class Span:
    '''A range of indices, `start <= i < end`.'''
    __slots__ = ('start', 'end')

    def __init__(self, start, end):
        assert start <= end
        self.start = start
        self.end = end

    def is_empty(self):
        return self.end == self.start

    def __len__(self):
        return self.end - self.start

    def __iter__(self):
        return iter(range(self.start, self.end))

    def __contains__(self, i):
        return self.start <= i < self.end

    def overlaps(self, other):
        '''Returns `True` if the two spans have at least one index in
        common.'''
        return other.start < self.end and self.start < other.end

    def overlaps_ends(self, other):
        '''Returns `True` if the spans overlap or touch at their endpoints.'''
        return other.start <= self.end and self.start <= other.end

    def intersect(self, other):
        '''Return the intersection of two spans.  Raises an exception if `not
        self.overlaps_ends(other)`.'''
        return Span(max(self.start, other.start), min(self.end, other.end))

    def contains(self, other):
        return self.start <= other.start and other.end <= self.end

    def __add__(self, x):
        return Span(self.start + x, self.end + x)

    def __sub__(self, x):
        return Span(self.start - x, self.end - x)

    def __str__(self):
        return 'Span(%d, %d)' % (self.start, self.end)

    def __repr__(self):
        return self.__str__()

    def copy(self):
        return Span(self.start, self.end)

def simplify_spans(ss):
    '''Simplify a list of spans by merging adjacent or overlapping spans
    together.

    Unlike most functions in this module, `simplify_spans` can handle inputs
    where the spans are overlapping (but they still must be sorted by start
    position).'''
    spans = []
    def merge(s):
        if len(spans) == 0:
            spans.append(s)
            return

        last = spans[-1]
        if s.start <= last.end:
            last.end = max(last.end, s.end)
        else:
            spans.append(s)

    for s in ss:
        merge(s)
    return spans

def merge_spans(ss1, ss2):
    '''Merge two lists of spans into a single list.'''
    simplify_input = []

    # Merge ss1 and ss2 into a single list ordered by `start`.  The result is
    # not valid for most purposes because it may contain overlapping spans, so
    # we run it through `simplify` at the end to clean it up.
    i = 0
    j = 0
    while i < len(ss1) and j < len(ss2):
        if ss1[i].start < ss2[j].start:
            simplify_input.append(ss1[i])
            i += 1
        else:
            simplify_input.append(ss2[j])
            j += 1

    simplify_input.extend(ss1[i:])
    simplify_input.extend(ss2[j:])

    print('presimplify: ', simplify_input)

    return simplify_spans(simplify_input)


NON_SPACE_RE = re.compile(r'[^ \n]+')

def detail_spans(s):
    i = 0
    spans = []
    while True:
        m = NON_SPACE_RE.search(s, i)
        if not m:
            break
        spans.append(Span(m.start(), m.end()))
        i = m.end()

    return spans


# A "diff" (according to this module) is a list of Context, Change, and Intra
# blocks, all non-overlapping, and covering the entirety of some range of old
# and new lines with no gaps.

# A block of context lines.  `old_lines` is a span of indices (0-based) into
# the old text; `new_lines` is indices in the new text.  Both have the same
# length, and the indicated lines have identical content.
Context = namedtuple('Context', ('old_lines', 'new_lines'))
# A block of changed lines.  `old_lines` and `new_lines` are spans, as in
# `Context`, but may differ in length and may refer to lines with unequal
# contents.
Change = namedtuple('Change', ('old_lines', 'new_lines'))
# A single line with intraline changes.  `old_line` and `new_line` are line
# numbers in the old/new text.  `del_spans` are spans of characters deleted
# from the old line; `ins_spans` are characters inserted in the new line.
Intra = namedtuple('Intra', ('old_line', 'new_line', 'del_spans', 'ins_spans'))

def block_len(b):
    if isinstance(b, Context):
        return len(b.old_lines)
    elif isinstance(b, Change):
        return max(len(b.old_lines), len(b.new_lines))
    elif isinstance(b, Intra):
        return 1
    else:
        raise TypeError('expected block, but got %s' % (type(b),))

def diff_len(blocks):
    return sum(block_len(b) for b in blocks)



# Basic diff construction.  This code is responsible for computing the actual
# changes between a pair of old and new texts.  The result is a list of
# Context, Change, and Intra blocks that covers the entirety of both inputs.

def diff_lines(old, new):
    '''Compute a diff of `old` and `new`, and yield a sequence of (old_line,
    new_line, old_detail, new_detail).  `line` is a boolean indicating whether
    there is a line present in the old/new file, and `detail` is a list of
    intraline edits.

    Possible outputs:
    - (True, True, None, None): Unmodified/context line
    - (True, False, None, None): Deletion of a line from the old text.
    - (False, True, None, None): Insertion of a line in the new text.
    - (True, True, [...], [...]): Changed line, modified via the indicated
      intraline insertions and deletions.
    '''
    old_lines = old.splitlines(keepends=True)
    new_lines = new.splitlines(keepends=True)

    # We buffer up to two previous result tuples.  This lets us handle
    # intraline change markers, and in particular, the nasty '-+?' case, where
    # we don't find out that we're in an intraline change ('?') until we've
    # seen both the '-' and '+' lines.
    buf = deque()

    for dl in difflib.ndiff(old_lines, new_lines):
        prefix = dl[0:2]

        if prefix == '  ':
            # Context line.  Flush the whole buffer.
            while buf:
                yield buf.popleft()
            yield (True, True, None, None)

        elif prefix == '- ':
            while buf:
                yield buf.popleft()
            buf.append((True, False, None, None))

        elif prefix == '+ ':
            # Try to fold into a previous intraline edit quad, if one exists.
            if len(buf) > 0:
                old_line, new_line, old_detail, new_detail = buf[-1]
                if not new_line and old_detail is not None:
                    # Previously saw a '-' and a '?'.  Fold in this '+'.
                    assert not new_line
                    buf[-1] = (old_line, True, old_detail, None)
                    continue
                # If there's no old_detail ('?'), then we aren't in an
                # intraline edit.  If there's a new_line, then the intraline
                # edit is already finished.  In either case, we want to do the
                # default action of just adding the '+' on its own.

            while len(buf) > 2:
                yield buf.popleft()
            buf.append((False, True, None, None))

        elif prefix == '? ':
            detail = detail_spans(dl[2:])

            # Add this detail to the previous buffered line.  We may also need
            # to merge a pair of previous '-' and '+' lines, if we didn't
            # previously know that they were part of an intraline change quad.
            assert len(buf) > 0
            old_line, new_line, old_detail, new_detail = buf.pop()

            if new_line:
                if old_line:
                    # The previous line is a rollup of a '-' and a '+'.
                    # (Context lines are not included in the buffer.)
                    assert old_detail is not None
                    buf.append((True, True, old_detail, detail))
                else:
                    # The previous line is just a '+'.  There must be a '-'
                    # before it, so roll up both of those together with the new
                    # detail.
                    old_line2, new_line2, old_detail2, new_detail2 = buf.pop()
                    assert old_line2
                    assert not new_line2
                    assert old_detail2 is None
                    assert new_detail2 is None
                    buf.append((True, True, None, detail))
            else:
                # The previous line is just a '-'.  Roll this detail into it.
                # Next we should see a '+', which will get rolled in, so this
                # bogus (True, False, [...], None) entry will never be yielded.
                buf.append((True, False, detail, None))

    while buf:
        yield buf.popleft()

def compute_diff(old, new):
    '''Build a diff between `old` and `new`.  The result is a list of
    blocks.'''
    in_context = True
    old_start = 0
    old_end = 0
    new_start = 0
    new_end = 0

    blocks = []

    def flush():
        nonlocal old_start, old_end, new_start, new_end
        if old_start == old_end and new_start == new_end:
            return

        if in_context:
            ctor = Context
        else:
            ctor = Change

        blocks.append(ctor(
            Span(old_start, old_end),
            Span(new_start, new_end)))
        old_start = old_end
        new_start = new_end

    def maybe_flush(new_in_context):
        nonlocal in_context
        if in_context != new_in_context:
            flush()
            in_context = new_in_context

    for old_line, new_line, old_detail, new_detail in diff_lines(old, new):
        if not old_line or not new_line:
            # Insertion or deletion
            maybe_flush(False)
            if old_line:
                old_end += 1
            if new_line:
                new_end += 1
        elif old_detail is not None or new_detail is not None:
            # Intraline change
            flush()
            blocks.append(Intra(old_start, new_start, old_detail, new_detail))
            old_end += 1
            old_start = old_end
            new_end += 1
            new_start = new_end
        else:
            # Context line
            maybe_flush(True)
            old_end += 1
            new_end += 1

    flush()
    return blocks



# Diff-line manipulation.  This code is used for determining which parts of the
# diff are visible and which are hidden.  It operates on lists of spans of
# "diff lines", where a diff line is a line as it will appear in the final
# two-column diff, consisting of an old line and a corresponding new line.
# (Either side's line may be missing, in the case of a Change block where
# `old_lines` and `new_lines` have different lengths.)
#
# Note that this code never explicitly constructs the two-column diff - it
# merely uses the two-column diff's line numbers to identify sections of the
# output.

def dl_change_spans(blocks):
    '''Return a list of spans of diff lines (0-based) covering all Change and
    Intra blocks.'''
    spans = []
    # Diff lines covered by the most recent run of `Change` and `Intra` blocks.
    # This can be empty if the most recent block was a `Context`.
    cur = Span(0, 0)

    for b in blocks:
        if isinstance(b, Context):
            if not cur.is_empty():
                spans.append(cur.copy())
            cur.end += len(b.old_lines)
            cur.start = cur.end
        elif isinstance(b, Change):
            cur.end += max(len(b.old_lines), len(b.new_lines))
        elif isinstance(b, Intra):
            cur.end += 1
        else:
            raise TypeError('expected block, but got %s' % (type(b),))

    if not cur.is_empty():
        spans.append(cur)
    return spans

def dl_unchanged_spans_inner(blocks, keep_spans, old):
    pos = 0
    result = []
    i = 0

    for b in blocks:
        if isinstance(b, Context):
            s = b.old_lines if old else b.new_lines

            while i < len(keep_spans) and keep_spans[i].end <= s.start:
                i += 1

            offset = s.start - pos
            for keep in keep_spans[i:]:
                if not keep.overlaps(s):
                    break
                overlap = keep.intersect(s)
                result.append(overlap - offset)

        pos += block_len(b)

    return result

def dl_unchanged_spans(blocks, old_spans, new_spans):
    '''Generate a list of diff line spans that covers all context lines whose
    old line falls within one of the `old_spans` or whose new line falls within
    one of the `new_spans`.  This is useful for forcing certain unchanged lines
    to appear in the diff, even when they're outside the context range of any
    changes.'''
    return merge_spans(
            dl_unchanged_spans_inner(blocks, old_spans, True),
            dl_unchanged_spans_inner(blocks, new_spans, False))

def dl_add_context(spans, context_lines, diff_end):
    '''Given a list of diff-line spans, expand each one by `context_lines` in
    both directions.  `diff_end` should be the number of lines in the diff -
    after expansion, spans will be truncated if they exceed this limit.'''
    new_spans = []

    for s in spans:
        ctx = Span(s.start - context_lines, s.end + context_lines)
        ctx.start = max(0, ctx.start)
        ctx.end = min(diff_end, ctx.end)

        if len(new_spans) == 0:
            new_spans.append(ctx)
            continue

        prev = new_spans[-1]
        # Note ctx.start is never less than prev.start, since they both had
        # context_lines subtracted and went through the same processing above.
        # Similarly, ctx.end is always >= prev.end, assuming the input is
        # well-formed (sorted and nonoverlapping).
        assert ctx.start >= prev.start and ctx.end >= prev.end
        if ctx.start in prev:
            prev.end = ctx.end
        else:
            new_spans.append(ctx)

    return new_spans

def split_hunks(blocks):
    '''Given a list of possibly non-contiguous blocks, split it into a list of
    contiguous diff hunks.'''
    old_pos = 0
    new_pos = 0
    cur = []
    hunks = []

    for b in blocks:
        if isinstance(b, (Context, Change)):
            old_span = b.old_lines
            new_span = b.new_lines
        elif isinstance(b, Intra):
            old_span = Span(b.old_line, b.old_line + 1)
            new_span = Span(b.new_line, b.new_line + 1)
        else:
            raise TypeError('expected block, but got %s' % (type(b),))

        if old_span.start != old_pos or new_span.start != new_pos:
            if len(cur) > 0:
                hunks.append(cur)
            cur = []

        cur.append(b)

        old_pos = old_span.end
        new_pos = new_span.end

    if len(cur) > 0:
        hunks.append(cur)
    return hunks

def dl_filter_blocks(blocks, spans):
    '''Filter a diff, extracting only those parts that are covered by one of
    the diff line spans in `spans`.  Since the resulting diff is likely
    non-contiguous, this function automatically splits the result into
    hunks.'''
    i = 0
    pos = 0

    # List of new blocks.  Blocks in this list aren't guaranteed to be
    # contiguous.
    new_blocks = []

    for b in blocks:
        block_span = Span(pos, pos + block_len(b))

        while i < len(spans) and spans[i].end <= block_span.start:
            i += 1

        if i < len(spans) and spans[i].contains(block_span):
            # Common case: the whole block is inside the filter span.
            new_blocks.append(b)
            pos += len(block_span)
            continue

        for s in spans[i:]:
            if s.is_empty():
                continue
            if not s.overlaps(block_span):
                break
            # `overlap` is a span relative to the block itself - its endpoints
            # are in the range 0 .. block_len(b).
            overlap = s.intersect(block_span) - block_span.start
            print('intersect %s with %s -> %s (%s)' % (s, block_span, overlap +
                block_span.start, overlap))

            if isinstance(b, Context):
                new_blocks.append(Context(
                    overlap + b.old_lines.start,
                    overlap + b.new_lines.start))
            elif isinstance(b, Change):
                new_blocks.append(Change(
                    (overlap + b.old_lines.start).intersect(b.old_lines),
                    (overlap + b.new_lines.start).intersect(b.new_lines)))
            elif isinstance(b, Intra):
                # Impossible: `Intra` is only one line, so if any part of it
                # `intersect`s the filter span, then the whole thing is inside
                # and should have been caught by the fast path above.
                assert False, 'unreachable'
            else:
                assert False, 'unreachable'

        pos += len(block_span)

    return split_hunks(new_blocks)



# Main entry point

def build_diff(old, new, context_lines=5, keep_old=[], keep_new=[]):
    '''Construct a diff between `old` and `new`, with `context_lines` of
    context around each change.  Also keep context lines where the old line
    falls in `keep_old` and/or the new line falls in `keep_new`.  The output is
    a list of hunks, where each hunk is a list of contiguous blocks.'''
    blocks = compute_diff(old, new)
    changes = dl_change_spans(blocks)
    changes_ctx = dl_add_context(changes, context_lines, diff_len(blocks))
    hunks = dl_filter_blocks(blocks, changes_ctx)
    return hunks



def test_dl(blocks):
    from pprint import pprint
    print('--------------')
    changes = dl_change_spans(blocks)
    pprint(('changes', changes))
    changes = dl_add_context(changes, 5, diff_len(blocks))
    pprint(('changes2', changes))
    keep = dl_unchanged_ranges(blocks, [
        Span(100, 200),
        Span(700, 720),
    ], [])
    pprint(('keep', keep))
    spans = merge_spans(changes, keep)
    pprint(('spans', spans))
    hunks = dl_filter_blocks(blocks, spans)
    pprint(('hunks', hunks))



def render_diff(files):
    file_names = sorted(files.keys())
    parts = []
    for f in file_names:
        old, new = files[f]
        test_dl(compute_diff(old, new))
        parts.extend(difflib.unified_diff(
            old.splitlines(keepends=True),
            new.splitlines(keepends=True),
            fromfile='old/%s' % f,
            tofile='new/%s' % f))
        parts.append('\n\n')

    return ''.join(parts)
