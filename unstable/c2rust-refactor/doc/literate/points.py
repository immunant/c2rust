'''
Labeled points.

This is similar to the `annot` module, but works with individual points instead
of spans.
'''
from typing import List, Tuple, Optional, Callable, Generic, TypeVar

from literate.annot import Span, Annot


T = TypeVar('T')
U = TypeVar('U')

class Point(Generic[T]):
    '''An index in some sequence, with a label applied.'''
    __slots__ = ('pos', 'label')

    def __init__(self, pos: int, label: T=None):
        self.pos = pos
        self.label = label

    def __add__(self, x: int) -> 'Point[T]':
        return Point(self.pos + x, self.label)

    def __sub__(self, x: int) -> 'Point[T]':
        return Point(self.pos - x, self.label)

    def __str__(self) -> str:
        return 'Point(%d, %r)' % (self.pos, self.label)

    def __repr__(self) -> str:
        return self.__str__()

    def copy(self) -> 'Point[T]':
        return Point(self.pos, self.label)

def annot_starts(annot: Annot[T]) -> List[Point[T]]:
    '''Get the start point of each span in `annot`, labeled with the span's
    original label.'''
    return [Point(s.start, s.label) for s in annot]

def annot_ends(annot: Annot[T]) -> List[Point[T]]:
    '''Get the end point of each span in `annot`, labeled with the span's
    original label.'''
    return [Point(s.end, s.label) for s in annot]

def annot_to_deltas(annot: Annot[T]) -> List[Point[Tuple[Optional[T], Optional[T]]]]:
    '''Turn an annotation into a list of points, where each point is on a span
    boundary and is labeled with the labels of the previous and next spans.'''
    if len(annot) == 0:
        return []

    result = []

    # The first span's start and the last span's end are special cases, since
    # they have no previous/next span to compare against.
    first = annot[0]
    result.append(Point(first.start, (None, first.label)))

    for (s1, s2) in zip(annot, annot[1:]):
        if s1.end == s2.start:
            # These spans are adjacent.  Record a transition directly from one
            # to the next at their shared boundary.
            result.append(Point(s1.end, (s1.label, s2.label)))
        else:
            # There is a gap between spans.  Record transitions to `None` and
            # back.
            result.append(Point(s1.end, (s1.label, None)))
            result.append(Point(s2.start, (None, s2.label)))

    # Note this works even when `len(annot) == 1` and thus `last is first`.
    last = annot[-1]
    result.append(Point(last.end, (last.label, None)))

    return result


def merge_points(p1: List[Point[T]], p2: List[Point[T]],
        *args: List[Point[T]]) -> List[Point[T]]:
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

def map_points(ps: List[Point[T]], f: Callable[[T], U]) -> List[Point[U]]:
    '''Map `f` over the labels of all points in `ps`.'''
    return [Point(p.pos, f(p.label)) for p in ps]

def cut_points(orig: List[Point[T]], cut: Annot[U],
        include_start: bool=True, include_end: bool=False) -> List[Tuple[Span[U], List[Point[T]]]]:
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

        # In order, handle:
        #  1. Points strictly before `cut_span`
        #  2. Points at `cut_span.start`
        #  3. Points strictly within `cut_span`
        #  4. Points at `cut_span.end` (only if `include_end` is `True`)

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
                # Rewind, so the same points can be processed by the next
                # iteration.  When `include_start` and `include_end` are both
                # set, and two spans are adjacent (`span1.end == span2.start`),
                # we want the points on the boundary to appear in both pieces.
                i = saved_i

        pieces.append((cut_span, acc))

    return pieces

def cut_annot_at_points(orig: Annot[T], cut: List[Point[U]]) -> Annot[T]:
    '''Cut the spans of annotation `orig` at each point in `cut`.  The
    resulting annotation applies all the same labels to the same regions as in
    `orig`, but any span that previously crossed a `cut` point is broken into
    two or more consecutive subspans.'''
    result = []
    def emit(s):
        # Filter out any zero-length spans.  This should only happen when two
        # points in `cut` occupy the same position.
        if len(s) > 0:
            result.append(s)

    i = 0
    for span in orig:
        # Skip points that lie strictly before `span`.
        while i < len(cut) and cut[i].pos <= span.start:
            i += 1

        # For each point that lies inside `span`, emit the subspan before the
        # point, then check for additional cut points in the subspan after the
        # point.
        while i < len(cut) and cut[i].pos < span.end:
            emit(Span(span.start, cut[i].pos, span.label))
            span = Span(cut[i].pos, span.end, span.label)
            i += 1

        emit(span)

    return result
