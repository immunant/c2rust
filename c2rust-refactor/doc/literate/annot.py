'''
Labeled spans and annotations.

A `Span` is a range of indices with an associated label.  Most commonly, these
are line or character indices in the text of some file.

An "annotation" is a list of spans, sorted by `start` position, with no
overlap.  An annotation is used to assign different labels to different parts
of the text.
'''

class Span:
    '''A range of indices, `start <= i < end`, with a label applied.'''
    __slots__ = ('start', 'end', 'label')

    def __init__(self, start, end, label=None):
        assert start <= end
        self.start = start
        self.end = end
        self.label = label

    def is_empty(self):
        return self.end == self.start

    def __len__(self):
        return self.end - self.start

    # A `Span` works like `range(start, end)` for iteration purposes

    def __iter__(self):
        return iter(range(self.start, self.end))

    def __contains__(self, i):
        '''Checks if index `i` falls within this span.'''
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
        self.overlaps_ends(other)`.  The result has the same `label` as
        `self`.'''
        return Span(
                max(self.start, other.start),
                min(self.end, other.end),
                self.label)

    def contains(self, other):
        '''Checks if span `other` is fully contained in `self`.'''
        return self.start <= other.start and other.end <= self.end

    def __add__(self, x):
        return Span(self.start + x, self.end + x, self.label)

    def __sub__(self, x):
        return Span(self.start - x, self.end - x, self.label)

    def __str__(self):
        return 'Span(%d, %d, %r)' % (self.start, self.end, self.label)

    def __repr__(self):
        return self.__str__()

    def copy(self):
        return Span(self.start, self.end, self.label)

def number_lines(lines: [str]) -> [Span]:
    '''Given a sequence of lines, return an annotation on the overall text
    (`''.join(lines)`) that labels the text of each line with its index in
    `lines`.  The resulting annotation covers the entire text without gaps.'''
    result = []
    pos = 0
    for i, l in enumerate(lines):
        result.append(Span(pos, pos + len(l), i))
        pos += len(l)
    return result

def cut_annot(orig: [Span], cut: [Span]) -> [(Span, [Span])]:
    '''Cut annotation `orig` into pieces, one for each span in `cut`.  Returns
    `len(cut)` pairs of (cut_span, annot), where `annot` is an annotation on
    the text that falls within `cut_span`.  The span positions in `annot` are
    adjusted to cover only the text within `cut_span`, so that a position of 0
    in `annot` corresponds to the start of `cut_span` in the overall text, and
    a position of `len(cut_span)` corresponds to `cut_span.end`.'''
    i = 0
    pieces = []

    for cut_span in cut:
        acc = []
        while i < len(orig):
            s = orig[i]
            if s.overlaps(cut_span):
                acc.append(s.intersect(cut_span) - cut_span.start)
            if s.end > cut_span.end:
                # `s` extends past the end of `cut_span`, potentially into the
                # next `cut_span`.  Keep it around for the next iteration.
                break
            i += 1
        pieces.append((cut_span, acc))

    return pieces

def merge_annot(a1: [Span], a2: [Span]) -> [Span]:
    '''Merge two annotations, producing one that includes all indices covered
    by either annotation.  The output spans will all have label `None`.'''
    result = SpanMerger()

    i1 = 0
    i2 = 0

    while i1 < len(a1) and i2 < len(a2):
        if a1[i1].start <= a2[i2].start:
            result.add(a1[i1])
            i1 += 1
        else:
            result.add(a2[i2])
            i2 += 1

    result.add_all(a1[i1:])
    result.add_all(a2[i2:])

    return result.finish()

def fill_annot(a: [Span], end: int, start=0, label=None) -> [Span]:
    '''Fill in any unannotated regions in `a` with the label `label`.  The
    result is an annotation that covers every position in the range `start ..
    end`, using labels from `a` when available, and using `label` otherwise.'''
    last_pos = start
    result = []
    for s in a:
        if s.start > last_pos:
            # There's a gap between `last_pos` and `s`.  Fill it with `label`.
            result.append(Span(last_pos, s.start, label))
        result.append(s)
        last_pos = s.end
    if end > last_pos:
        result.append(Span(last_pos, end, label))
    return result

def zip_annot(a1: [Span], a2: [Span], f=lambda l1, l2: (l1, l2)) -> [Span]:
    '''Zip together two annotations, returning an annotation that labels each
    position with a pair `(l1, l2)`, where `l1` is the position's label in `a1`
    and `l2` is its label in `a2`.  Only positions with labels in both `a1` and
    `a2` will have labels in the output annotation (preprocess with
    `fill_annot` if this is not what you want).'''
    result = []
    for s2, ss1 in cut_annot(a1, a2):
        for s1 in ss1:
            start = s1.start + s2.start
            end = s1.end + s2.start
            result.append(Span(start, end, f(s1.label, s2.label)))
    return result

def lookup_span(a: [Span], pos: int,
        include_start=True, include_end=False) -> Span:
    '''Get the span in `a` that contains `pos`, or `None` if there is no such
    span.'''
    # `bisect` doesn't support a key function, so we just do a linear scan.
    for s in a:
        if s.end > pos or (include_end and s.end == pos):
            if s.start < pos or (include_start and s.start == pos):
                return s
            else:
                return None
    return None

class SpanMerger:
    '''Helper for building a valid annotation from a sorted sequence of
    possibly-overlapping spans.
    
    Note that this class may mutate the spans provided to `add`.'''
    def __init__(self):
        self.acc = []

    def add(self, span):
        '''Add `span` to the result sequnece, merging it with the previous span
        if it overlaps.  In case of overlap, the merged span retains the label
        of the first span provided with `add`.'''
        if len(self.acc) > 0 and span.start <= self.acc[-1].end:
            self.acc[-1].end = max(self.acc[-1].end, span.end)
        else:
            self.acc.append(span)

    def add_all(self, spans):
        for s in spans:
            self.add(s)

    def finish(self):
        result = self.acc
        self.acc = None
        return result
