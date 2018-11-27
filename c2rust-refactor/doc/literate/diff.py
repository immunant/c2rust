from collections import namedtuple, deque
import difflib
import pygments.formatters
import pygments.lexers
import pygments.token
import re

from literate.annot import Span, merge_annot, SpanMerger
from literate.file import File, Diff, Hunk, OutputLine


# Regex for finding runs of identical non-space characters
RUN_RE = re.compile(r'([^ \n])\1*')

def parse_intra_annot(s):
    '''Parse an `ndiff` detail (`?`) line and convert it to an annotation
    indicating intraline edits in the text of the preceding line.  The
    annotation labels inserted, deleted, and changed characters with `'ins'`,
    `'del'`, and `'chg'` respectively.'''
    spans = []
    for m in RUN_RE.finditer(s):
        c = m.group(1)
        # Map the symbols used by `ndiff` to something more meaningful.
        label = {
                '+': 'ins',
                '-': 'del',
                '^': 'chg',
                }[c]
        spans.append(Span(m.start(), m.end(), label))
    return spans

def diff_lines(old_lines: [str], new_lines: [str]):
    '''Compute a diff of `old` and `new`, and yield a sequence of (old_line,
    new_line, old_detail, new_detail).  Each `line` is a boolean indicating
    whether there is a line present in the old/new file, and each `detail` is
    an intraline edit annotation (see `parse_intra_annot`).

    Possible outputs:
    - (True, True, None, None): Unmodified/context line
    - (True, False, None, None): Deletion of a line from the old text.
    - (False, True, None, None): Insertion of a line in the new text.
    - (True, True, [...], [...]): Changed line, modified via the indicated
      intraline insertions and deletions.
    '''
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
            detail = parse_intra_annot(dl[2:])

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

    # Flush any remaining buffered entries.
    while buf:
        yield buf.popleft()

def adjust_closing_brace(old_lines: [str], new_lines: [str], diff):
    '''Adjust the output of `diff_lines` to turn this:

         fn f() {
           ...
        +}
        +fn g() {
        +  ...
         }

    into this:

         fn f() {
           ...
         }
        +fn g() {
        +  ...
        +}
    '''
    # Specifically: at the end of every run of insertions or deletions, if the
    # first context line after the run consists of solely a '}' character (with
    # whitespace), then we scan from the top of the run for an identical
    # inserted line.  If found, we change the earlier line from an insertion to
    # context, and change the context line to an insertion.

    mode = None
    buf = []
    buf_start = None

    old_i = -1
    new_i = -1

    for dl in diff:
        old_line, new_line, old_detail, new_detail = dl
        if old_line and not new_line:
            new_mode = 'del'
            old_i += 1
        elif not old_line and new_line:
            new_mode = 'ins'
            new_i += 1
        else:
            new_mode = None
            old_i += 1
            new_i += 1

        if new_mode != mode:
            if new_mode is None:
                # Switching from ins or del mode to context mode.  If the
                # current line is a '}', we try to do the block adjustment.
                check_lines = new_lines if mode == 'ins' else old_lines
                i = new_i if mode == 'ins' else old_i
                if check_lines[i].strip() == '}':
                    # Yield everything from buf, while scanning for an earlier
                    # matching line.
                    found_dl = None
                    for j, buf_dl in enumerate(buf):
                        if check_lines[buf_start + j] == check_lines[i]:
                            found_dl = buf_dl
                            yield (True, True, None, None)
                            # We're stopping early, so yield the remaining
                            # elements.
                            yield from buf[j + 1:]
                            break
                        else:
                            yield buf_dl
                    if found_dl:
                        yield found_dl
                    else:
                        yield (True, True, None, None)
                else:
                    yield from buf
                    yield dl
                mode = None
                buf = []
                buf_start = None
                # We already yielded the correct info, so don't fall through to
                # the default logic.
                continue
            else:
                if mode is not None:
                    yield from buf
                mode = new_mode
                buf = []
                buf_start = new_i if mode == 'ins' else old_i

        if mode is None:
            yield dl
        else:
            buf.append(dl)

    # There are no more lines, so there can't be a `}` line following `buf` to
    # trigger our heuristic.  That means we can blindly dump everything in
    # `buf`.
    yield from buf

def diff_files(f1: File, f2: File) -> Diff:
    '''Diff two files, returning a `Diff` between them and also setting the
    `intra` annotation on the lines of both files.'''
    dls = diff_lines(f1.line_text, f2.line_text)
    dls = adjust_closing_brace(f1.line_text, f2.line_text, dls)

    # Accumulator for diff blocks.
    diff_blocks = []

    # Start and current position of the current block.
    old_start = 0
    old_cur = 0
    new_start = 0
    new_cur = 0
    # Is the current block a change?  (If not, it's context.)
    changed = True

    def flush():
        nonlocal old_start, new_start
        # This check means we can blindly call `flush()` without worrying about
        # cluttering the output with zero-length blocks.
        if old_cur - old_start > 0 or new_cur - new_start > 0:
            diff_blocks.append((changed,
                Span(old_start, old_cur),
                Span(new_start, new_cur)))
        old_start = old_cur
        new_start = new_cur

    for old_line, new_line, old_detail, new_detail in dls:
        next_changed = not (old_line and new_line and
                old_detail is None and new_detail is None)
        has_intra = old_detail is not None or new_detail is not None
        if next_changed != changed:
            flush()

        if has_intra:
            # Emit each `intra` line as its own block, to ensure they're
            # aligned in the output.
            flush()
            if old_detail is not None:
                f1.lines[old_cur].set_intra(old_detail)
            if new_detail is not None:
                f2.lines[new_cur].set_intra(new_detail)
            flush()

        if old_line:
            old_cur += 1
        if new_line:
            new_cur += 1
        changed = next_changed

    flush()

    return Diff(f1, f2, diff_blocks)


def context_annot(blocks: [(bool, Span, Span)], new: bool, context_lines: int) -> [Span]:
    '''Generate an annotation of the old or new file's lines, indicating which
    lines are changes or context for changes (within `context_lines`
    distance).'''
    result = SpanMerger()

    for (changed, old_span, new_span) in blocks:
        if not changed:
            continue

        span = new_span if new else old_span
        result.add(Span(
            span.start - context_lines,
            span.end + context_lines))

    return result.finish()

def filter_unchanged(blocks: [(bool, Span, Span)],
        old_filt: [Span], new_filt: [Span]) -> [(bool, Span, Span)]:
    '''Filter `blocks`, keeping changed blocks along with any portions of
    unchanged blocks that fall within `old_filt` or `new_filt`.  The result is
    formatted like `Diff.blocks` but blocks may not be contiguous and may not
    cover the entire old/new file.'''
    result = []

    old_i = 0
    new_i = 0

    for changed, old_span, new_span in blocks:
        if changed:
            result.append((changed, old_span, new_span))
            continue

        # In unchanged blocks, the old and new text are identical, so the spans
        # should contain the same number of lines.
        assert len(old_span) == len(new_span)

        # Subspans of the current block that we should keep.  In these
        # annotations, position 0 represents the start of the block and
        # `len(old_span)` / `len(new_span)` represents its end.
        old_keep = []
        new_keep = []

        while old_i < len(old_filt):
            s = old_filt[old_i]
            if s.overlaps(old_span):
                old_keep.append(s.intersect(old_span) - old_span.start)
            if s.end > old_span.end:
                # `s` extends past the end of `old_span`, potentially into the
                # next block's `old_span`.  Keep it around for the next block
                # to see.
                break
            old_i += 1

        while new_i < len(new_filt):
            s = new_filt[new_i]
            if s.overlaps(new_span):
                new_keep.append(s.intersect(new_span) - new_span.start)
            if s.end > new_span.end:
                break
            new_i += 1

        keep = merge_annot(old_keep, new_keep)
        for s in keep:
            result.append((False, s + old_span.start, s + new_span.start))

    return result

def split_hunks(blocks: [(bool, Span, Span)]) -> [Hunk]:
    '''Split the output of `filter_unchanged` into hunks, anywhere there's a
    gap in the old or new line numbers.'''
    last_old = 0
    last_new = 0
    cur = []
    hunks = []

    def flush():
        nonlocal cur
        if len(cur) > 0:
            hunks.append(Hunk(cur))
        cur = []

    for b in blocks:
        changed, old_span, new_span = b
        if old_span.start != last_old or new_span.start != last_new:
            flush()
        cur.append(b)
        last_old = old_span.end
        last_new = new_span.end

    flush()
    return hunks

def build_diff_hunks(d: Diff):
    '''Build a list of output hunks, and assign it to `d.hunks`.

    If `d.old_file` or `d.new_file` has a `keep_mark_lines` annotation, all
    annotated lines will be kept as additional context.'''
    keep_old = context_annot(d.blocks, False, 5)
    if d.old_file.keep_mark_lines is not None:
        keep_old = merge_annot(keep_old, d.old_file.keep_mark_lines)

    keep_new = context_annot(d.blocks, True, 5)
    if d.new_file.keep_mark_lines is not None:
        keep_new = merge_annot(keep_new, d.new_file.keep_mark_lines)

    blocks = filter_unchanged(d.blocks, keep_old, keep_new)
    hunks = split_hunks(blocks)
    d.set_hunks(hunks)


def hunk_output_lines(h: Hunk) -> [OutputLine]:
    result = []
    for changed, old_span, new_span in h.blocks:
        common_lines = min(len(old_span), len(new_span))
        for i in range(0, common_lines):
            result.append(OutputLine(changed, old_span.start + i, new_span.start + i))
        for i in range(common_lines, len(old_span)):
            result.append(OutputLine(changed, old_span.start + i, None))
        for i in range(common_lines, len(new_span)):
            result.append(OutputLine(changed, None, new_span.start + i))
    return result

def build_output_lines(d: Diff):
    '''Build a list of two-column output lines for each hunk of `d`, and set
    the `Hunk.output_lines` fields.'''
    for h in d.hunks:
        output_lines = hunk_output_lines(h)
        h.set_output_lines(output_lines)
