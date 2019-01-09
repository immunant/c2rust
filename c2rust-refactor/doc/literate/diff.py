from collections import namedtuple, deque
import difflib
import pygments.formatters
import pygments.lexers
import pygments.token
import re
from typing import List, Tuple, Optional, Iterator, Iterable

from literate.annot import Span, Annot, SpanMerger, \
        cut_annot, merge_annot, sub_annot, fill_annot
from literate.file import File, Line, Diff, DiffBlock, Hunk, OutputLine
from literate.points import Point, cut_annot_at_points


# Regex for finding runs of identical non-space characters
RUN_RE = re.compile(r'([^ \n])\1*')

def parse_intra_annot(s: str) -> Annot[str]:
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


DiffLine = Tuple[bool, bool, Optional[Annot[str]], Optional[Annot[str]]]

def diff_lines(old_lines: List[str], new_lines: List[str]) -> Iterator[DiffLine]:
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

def adjust_closing_brace(old_lines: List[str], new_lines: List[str],
        diff: Iterable[DiffLine]) -> Iterator[DiffLine]:
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

WORD_BREAK_RE = re.compile(r'\b')

def token_annot(line: Line) -> Annot[None]:
    '''Annotate the tokens of `l`.  Each token (and some sub-token strings)
    gets a separate span.  This is a helper function for
    `calc_tokenized_intra`.'''
    annot = fill_annot(line.highlight, len(line.text))

    # Special cases: treat word boundaries inside strings and comments as token
    # breaks.  This essentially gives us the behavior of `git`'s `--word-diff`
    # feature.
    extra_cuts = []
    for span in annot:
        # We don't handle String subtypes (only String itself) because we don't
        # want to break up `\x00` and similar escapes.
        if span.label == pygments.token.String or \
                span.label in pygments.token.Comment:
            text = line.text[span.start : span.end]
            for m in WORD_BREAK_RE.finditer(text):
                extra_cuts.append(Point(span.start + m.start()))

    return cut_annot_at_points(annot, extra_cuts)

def calc_tokenized_intra(l1: Line, l2: Line) -> Tuple[Annot[str], Annot[str]]:
    '''Calculate token-based intraline edit annotations for `l1` and `l2`.

    `difflib.ndiff` does a pretty good job of matching up similar lines, but it
    computes intraline changes character-by-character, which often produces bad
    results.  For example, it might turn `unsafe` into `malloc` by replacing
    `uns` -> `m` and `fe` -> `lloc`, instead of doing `unsafe` -> `malloc` in
    one go.

    Here we calculate some intraline edits that are easier to read, using the
    tokenization provided by `pygments` to align edit boundaries to the
    boundaries of source tokens.'''
    annot1 = token_annot(l1)
    annot2 = token_annot(l2)

    tokens1 = [l1.text[s.start : s.end] for s in annot1]
    tokens2 = [l2.text[s.start : s.end] for s in annot2]

    intra1 = []
    intra2 = []

    sm = difflib.SequenceMatcher(a=tokens1, b=tokens2)
    for tag, i1, i2, j1, j2 in sm.get_opcodes():
        if tag == 'equal':
            continue

        while i1 < i2 and tokens1[i1].isspace():
            i1 += 1
        while i2 > i1 and tokens1[i2 - 1].isspace():
            i2 -= 1

        while j1 < j2 and tokens2[j1].isspace():
            j1 += 1
        while j2 > j1 and tokens2[j2 - 1].isspace():
            j2 -= 1

        if i1 != i2:
            intra1.append(Span(annot1[i1].start, annot1[i2 - 1].end,
                'chg' if tag == 'replace' else 'del'))

        if j1 != j2:
            intra2.append(Span(annot2[j1].start, annot2[j2 - 1].end,
                'chg' if tag == 'replace' else 'ins'))

    return (intra1, intra2)

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
            diff_blocks.append(DiffBlock(changed,
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
            intra1, intra2 = calc_tokenized_intra(
                    f1.lines[old_cur], f2.lines[new_cur])
            if len(intra1) > 0:
                f1.lines[old_cur].set_intra(intra1)
            if len(intra2) > 0:
                f2.lines[new_cur].set_intra(intra2)
            flush()

        if old_line:
            old_cur += 1
        if new_line:
            new_cur += 1
        changed = next_changed

    flush()

    return Diff(f1, f2, diff_blocks)


def context_annot(blocks: List[DiffBlock], new: bool, context_lines: int) -> Annot[None]:
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

def split_hunks(blocks: List[DiffBlock]) -> List[Hunk]:
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

def annotate_blocks(blocks: List[DiffBlock]) \
        -> Tuple[Annot[Span[None]], Annot[Span[None]]]:
    '''Return annotations on the old and new files, labeling each line with the
    block that contains it.'''
    old = []
    new = []
    for b in blocks:
        old.append(Span(b.old_span.start, b.old_span.end, b))
        new.append(Span(b.new_span.start, b.new_span.end, b))
    return old, new

def build_diff_hunks(d: Diff, context_diff: bool=True):
    '''Build a list of output hunks, and assign it to `d.hunks`.

    If `d.old_file` or `d.new_file` has a `keep_mark_lines` annotation, all
    annotated lines will be kept as additional context.'''
    # Find the set of lines each file wants to keep.
    def calc_file_keep(f, is_new):
        if context_diff:
            keep = context_annot(d.blocks, is_new, 5)
            if f.keep_mark_lines is not None:
                keep = merge_annot(keep, f.keep_mark_lines)
        else:
            if len(f.line_annot) > 0:
                keep = [Span(0, f.line_annot[-1].end)]
            else:
                keep = []
        if f.drop_irrelevant_lines is not None:
            keep = sub_annot(keep, f.drop_irrelevant_lines)

        return keep

    keep_old = calc_file_keep(d.old_file, False)
    keep_new = calc_file_keep(d.new_file, True)

    # In unchanged blocks, add each file's keep lines to the other file's set.
    # This works because unchanged blocks have the same number of lines on each
    # side.
    old_blocks, new_blocks = annotate_blocks(d.blocks)
    extra_keep_old = []
    extra_keep_new = []
    for block_span, keep_spans in cut_annot(keep_old, old_blocks):
        if block_span.label.changed:
            continue
        base = block_span.label.new_span.start
        extra_keep_new.extend(s + base for s in keep_spans)
    for block_span, keep_spans in cut_annot(keep_new, new_blocks):
        if block_span.label.changed:
            continue
        base = block_span.label.old_span.start
        extra_keep_old.extend(s + base for s in keep_spans)

    keep_old = merge_annot(keep_old, extra_keep_old)
    keep_new = merge_annot(keep_new, extra_keep_new)

    # For changed blocks, we can't match up lines from different files, so we
    # just hope for the best.  (Normally all changed lines are kept, so there's
    # no need to match - the only exception is when the `irrelevant_*_regex`
    # options are set.)

    # Build the filtered list of blocks.  There can be different numbers of
    # blocks on the old and new sides.  We use a fairly naive strategy to match
    # them up, but it generally seems to work okay.

    blocks = []
    for (old_block, old_keeps), (new_block, new_keeps) in zip(
            cut_annot(keep_old, old_blocks),
            cut_annot(keep_new, new_blocks)):
        # `old_blocks` and `new_blocks` have corresponding entries (based on
        # the same block) at corresponding positions.
        assert old_block.label is new_block.label
        block = old_block.label

        # Match up `old_keeps` and `new_keeps` entries by position.  In most
        # cases, the two lists will have the same length.
        for old_keep, new_keep in zip(old_keeps, new_keeps):
            blocks.append(DiffBlock(block.changed,
                old_keep + block.old_span.start,
                new_keep + block.new_span.start))
        for old_keep in old_keeps[len(new_keeps):]:
            blocks.append(DiffBlock(block.changed,
                old_keep + block.old_span.start,
                Span(block.new_span.end, block.new_span.end)))
        for new_keep in new_keeps[len(old_keeps):]:
            blocks.append(DiffBlock(block.changed,
                Span(block.old_span.end, block.old_span.end),
                new_keep + block.new_span.start))

    # Split the new blocks into hunks, and save them in the `Diff`.
    hunks = split_hunks(blocks)
    d.set_hunks(hunks)


def hunk_output_lines(h: Hunk) -> List[OutputLine]:
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
