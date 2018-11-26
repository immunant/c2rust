from literate.annot import Span

class Point:
    '''An index in some sequence, with a label applied.'''
    __slots__ = ('pos', 'label')

    def __init__(self, pos, label=None):
        self.pos = pos
        self.label = label

    def __add__(self, x):
        return Point(self.pos + x, self.label)

    def __sub__(self, x):
        return Point(self.pos - x, self.label)

    def __str__(self):
        return 'Point(%d, %r)' % (self.pos, self.label)

    def __repr__(self):
        return self.__str__()

    def copy(self):
        return Point(self.pos, self.label)

def annot_starts(annot: [Span]) -> [Point]:
    return [Point(s.start, s.label) for s in annot]

def annot_ends(annot: [Span]) -> [Point]:
    return [Point(s.end, s.label) for s in annot]

def annot_to_deltas(annot: [Span]) -> [Point]:
    '''Turn an annotation into a list of points, where each point is on a span
    boundary and is labeled with the labels of the previous and next spans.'''
    if len(annot) == 0:
        return []

    result = []

    first = annot[0]
    result.append(Point(first.start, (None, first.label)))

    for (s1, s2) in zip(annot, annot[1:]):
        if s1.end == s2.start:
            result.append(Point(s1.end, (s1.label, s2.label)))
        else:
            result.append(Point(s1.end, (s1.label, None)))
            result.append(Point(s2.start, (None, s2.label)))

    last = annot[-1]
    result.append(Point(last.end, (last.label, None)))

    return result


def merge_points(p1: [Point], p2: [Point], *args) -> [Point]:
    '''Merge two (sorted) lists of points, returning a sorted result.  If `p1`
    and `p2` have points at the same position, the result will contain all the
    `p1` points at that position (in their original order), followed by all the
    ones from `p2`.'''
    if len(args) > 0:
        acc = merge_points(p1, p2)
        for ps in args:
            acc = merge_points(acc, ps)
        return acc

    i1 = 0
    i2 = 0

    result = []

    while i1 < len(p1) and i2 < len(p2):
        if p1[i1].pos <= p2[i2].pos:
            result.append(p1[i1])
            i1 += 1
        else:
            result.append(p2[i2])
            i2 += 1

    result.extend(p1[i1:])
    result.extend(p2[i2:])

    return result

def map_points(ps: [Point], f) -> [Point]:
    '''Map `f` over the labels of all points in `ps`.'''
    return [Point(p.pos, f(p.label)) for p in ps]

def cut_points(orig: [Point], cut: [Span],
        include_start=True, include_end=False) -> [(Span, [Point])]:
    '''Cut a list of points `orig` into pieces, one for each span in `Cut`.
    Returns `len(cut)` pairs of (cut_span, points), where `points` is a subset
    of `orig` that falls within `cut_span`.  Position 0 in `points` corresponds
    to `cut_span.start` in the overall text.
    
    By default, a point lying on `cut_span`'s start is included in the
    `points`, while a point lying on its end is not.  This can be configured
    with `include_start` and `include_end`.  Note that setting both
    `include_start` and `include_end` to `True` can cause a point to appear
    twice in the output, if it falls on the boundary between two adjacent cut
    spans.'''

    i = 0
    pieces = []

    for cut_span in cut:
        acc = []
        def emit(p):
            acc.append(p - cut_span.start)

        while i < len(orig) and orig[i].pos < cut_span.start:
            i += 1

        while i < len(orig) and orig[i].pos == cut_span.start:
            if include_start:
                emit(orig[i])
            i += 1

        while i < len(orig) and orig[i].pos < cut_span.end:
            emit(orig[i])
            i += 1

        if include_end:
            saved_i = i
            while i < len(orig) and orig[i].pos == cut_span.end:
                emit(orig[i])
                i += 1
            if include_start:
                # Rewind, in case the current span's end overlaps with the next
                # span's start.  This lets the points appear in both pieces.
                i = saved_i

        pieces.append((cut_span, acc))

    return pieces

