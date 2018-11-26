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
        self.overlaps_ends(other)`.  The result has the same `label` as
        `self`.'''
        return Span(
                max(self.start, other.start),
                min(self.end, other.end),
                self.label)

    def contains(self, other):
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
    result = []
    pos = 0
    for i, l in enumerate(lines):
        result.append(Span(pos, pos + len(l), i))
        pos += len(l)
    return result

def cut_annot(orig: [Span], cut: [Span]) -> [(Span, [Span])]:
    '''Cut annotation `orig` into pieces, one for each span in `cut`.  Returns
    `len(cut)` pairs of (cut_span, annot), where `annot` is an annotation on
    the text that falls within `cut_span`.  Position 0 in `annot` corresponds
    to `cut_span.start` in the overall text.'''
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
    result = []
    def add(span):
        if len(result) > 0 and span.start <= result[-1].end:
            result[-1].end = max(result[-1].end, span.end)
        else:
            result.append(span)

    i1 = 0
    i2 = 0

    while i1 < len(a1) and i2 < len(a2):
        if a1[i1].start <= a2[i2].start:
            add(a1[i1])
            i1 += 1
        else:
            add(a2[i2])
            i2 += 1

    for s in a1[i1:]:
        add(s)

    for s in a2[i2:]:
        add(s)

    return result

def fill_annot(a: [Span], end: int, start=0, label=None) -> [Span]:
    '''Fill in any unannotated regions in `a` with the label `label`.  The
    result is an annotation that covers every position in the range `start ..
    end`.'''
    last_pos = start
    result = []
    for s in a:
        if s.start > last_pos:
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
    `a2` will have labels in the output annotation.'''
    result = []
    for s2, ss1 in cut_annot(a1, a2):
        for s1 in ss1:
            start = s1.start + s2.start
            end = s1.end + s2.start
            result.append(Span(start, end, f(s1.label, s2.label)))
    return result
