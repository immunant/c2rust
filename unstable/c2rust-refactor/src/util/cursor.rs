//! Defines `Cursor`, a helper type for editing sequences.
use log::info;

/// A `Cursor` indicates a position within a sequence of `T`s.  The position can be anywhere within
/// the sequence, including before the first item and after the last.
///
/// # Marks
///
/// In addition to moving through and manipulating the sequence, `Cursor`s  also permit marking the
/// current position so it can be referenced in later operations.  These marks remain in place even
/// if items are inserted, removed, or replaced before or after the mark.  (But replacing a range
/// of items will invalidate all marks between the replaced items.)
pub struct Cursor<T> {
    /// Stores elements left of the current position.
    left: Vec<T>,
    /// Stores elements right of the current position, in reverse order.
    right: Vec<T>,
    /// Stores marks left of the current position.
    left_marks: Vec<MarkData>,
    /// Stores marks right of the current position, in reverse order.
    right_marks: Vec<MarkData>,

    next_mark: usize,
}

struct MarkData {
    name: usize,

    /// The distance of this mark from the next mark closer to the center.  For example, if `m` is
    /// in `left_marks` and `m.depth` is 3, then `m` is 3 positions away from the mark to its
    /// right.  For the purposes of this field, the `Cursor`'s current position is treated as a
    /// mark, so if the last `Mark` in `left_marks` has `depth` 5, that mark is 5 positions to the
    /// left of the current position.
    depth: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Mark(usize);

#[allow(dead_code)]
impl<T> Cursor<T> {
    pub fn new() -> Cursor<T> {
        Cursor {
            left: Vec::new(),
            right: Vec::new(),
            left_marks: Vec::new(),
            right_marks: Vec::new(),
            next_mark: 0,
        }
    }

    pub fn from_vec(orig: Vec<T>) -> Cursor<T> {
        let mut orig = orig;
        orig.reverse();
        Cursor {
            left: Vec::new(),
            right: orig,
            left_marks: Vec::new(),
            right_marks: Vec::new(),
            next_mark: 0,
        }
    }

    pub fn into_vec(mut self) -> Vec<T> {
        self.right.reverse();
        self.left.append(&mut self.right);
        self.left
    }

    /// Move the cursor back by one position.
    pub fn retract(&mut self) {
        let x = self
            .left
            .pop()
            .expect("retract: already at start of buffer");
        self.right.push(x);

        while self.left_marks.last().map_or(false, |m| m.depth == 0) {
            let m = self.left_marks.pop().unwrap();
            self.right_marks.push(m);
        }

        if let Some(m) = self.left_marks.last_mut() {
            m.depth -= 1;
        }

        if let Some(m) = self.right_marks.last_mut() {
            m.depth += 1;
        }
    }

    /// Move the cursor back by `n` positions.
    pub fn retract_by(&mut self, n: usize) {
        // TODO: Could probably optimize this, particularly the mark-list manipulation part.
        for _ in 0..n {
            self.retract();
        }
    }

    /// Move the cursor forward by one position.
    pub fn advance(&mut self) {
        let x = self.right.pop().expect("advance: already at end of buffer");
        self.left.push(x);

        while self.right_marks.last().map_or(false, |m| m.depth == 0) {
            let m = self.right_marks.pop().unwrap();
            self.left_marks.push(m);
        }

        if let Some(m) = self.right_marks.last_mut() {
            m.depth -= 1;
        }

        if let Some(m) = self.left_marks.last_mut() {
            m.depth += 1;
        }
    }

    /// Move the cursor forward by `n` positions.
    pub fn advance_by(&mut self, steps: usize) {
        // TODO: Optimize
        for _ in 0..steps {
            self.advance();
        }
    }

    /// Return a `Mark` indicating the current position.  As long as the mark remains valid, `seek`
    /// can be used to return to this position.
    pub fn mark(&mut self) -> Mark {
        let name = self.next_mark;
        self.next_mark += 1;

        self.left_marks.push(MarkData { name, depth: 0 });
        Mark(name)
    }

    /// Return to a previously marked position.  Panics if the mark is invalid.
    pub fn seek(&mut self, mark: Mark) {
        if let Some(depth) = find_mark_depth(&self.left_marks, mark) {
            self.retract_by(depth);
        } else if let Some(depth) = find_mark_depth(&self.right_marks, mark) {
            self.advance_by(depth);
        } else {
            panic!("mark is no longer valid for this sequence");
        }
    }

    /// Checks if the cursor is positioned at the end of the sequence.
    pub fn eof(&self) -> bool {
        self.right.is_empty()
    }

    /// Insert an element at the current position.  Afterward, the cursor will point after the
    /// inserted element.
    pub fn insert(&mut self, x: T) {
        self.left.push(x);
        if let Some(m) = self.left_marks.last_mut() {
            m.depth += 1;
        }
    }

    /// Insert several elements at the current position.  Afterward, the cursor will point after
    /// the inserted elements.
    pub fn insert_multi(&mut self, xs: Vec<T>) {
        let n = xs.len();
        let mut xs = xs;
        self.left.append(&mut xs);

        if let Some(m) = self.left_marks.last_mut() {
            m.depth += n;
        }
    }

    /// Remove the element after the current position.  Panics if the current position is at the
    /// end of the sequence.
    pub fn remove(&mut self) -> T {
        let x = self.right.pop().expect("remove: already at end of buffer");

        // Marks on the right move 1 position closer to the center.
        while self.right_marks.last().map_or(false, |m| m.depth == 0) {
            let m = self.right_marks.pop().unwrap();
            self.left_marks.push(m);
        }

        if let Some(m) = self.right_marks.last_mut() {
            m.depth -= 1;
        }

        x
    }

    /// Remove `n` elements after the current position.
    pub fn remove_multi(&mut self, n: usize) -> Vec<T> {
        assert!(
            n <= self.right.len(),
            "remove_multi: too close to end of buffer"
        );
        let at = self.right.len() - n;
        let mut xs = self.right.split_off(at);
        xs.reverse();
        let xs = xs;

        // Move all marks at the current position into `left_marks`.
        while self.right_marks.last().map_or(false, |m| m.depth == 0) {
            let m = self.right_marks.pop().unwrap();
            self.left_marks.push(m);
        }

        // Remove all marks that lie within the removed range.
        let mut mark_moves = n;
        while self
            .right_marks
            .last()
            .map_or(false, |m| m.depth < mark_moves)
        {
            let m = self.right_marks.pop().unwrap();
            mark_moves -= m.depth;
        }

        if let Some(m) = self.right_marks.last_mut() {
            m.depth -= mark_moves;
        }

        xs
    }

    /// Return a reference to the element immediately after the current position.
    pub fn next(&self) -> &T {
        self.right.last().expect("peek: at end of buffer")
    }

    /// Advance until an element matches `pred`.  The cursor is left pointing just before the
    /// matching element.  If no element matches, the cursor points at the end of the sequence.
    pub fn advance_until<F: FnMut(&T) -> bool>(&mut self, pred: F) {
        let n = if let Some(n) = find_matching_rev(&self.right, pred) {
            n
        } else {
            self.right.len()
        };

        self.advance_by(n);
    }

    /// Remove elements between the current position and the given mark.  The mark must not be
    /// behind the current position in the buffer.
    pub fn remove_to_mark(&mut self, m: Mark) -> Vec<T> {
        let depth = if let Some(depth) = find_mark_depth(&self.right_marks, m) {
            depth
        } else if let Some(depth) = find_mark_depth(&self.left_marks, m) {
            if depth == 0 {
                0
            } else {
                panic!("remove_to_mark: mark is earlier in buffer than the current position");
            }
        } else {
            panic!("remove_to_mark: mark is no longer valid for buffer");
        };

        self.remove_multi(depth)
    }

    /// Replace the element after the cursor by transforming it with `func`.  Does not move the
    /// cursor.
    pub fn replace<F>(&mut self, func: F)
    where
        F: FnOnce(T) -> T,
    {
        // Don't use `remove` / `insert` to ensure that marks remain in place.
        let old = self.right.pop().expect("replace: at end of buffer");
        let new = func(old);
        self.right.push(new);
    }

    /// Extract the elements between `start` and `end`, transform them using `func`, and insert the
    /// results in their place.  Afterward, the cursor points past the end of the replacement
    /// sequence.
    pub fn replace_range<F>(&mut self, start: Mark, end: Mark, func: F)
    where
        F: FnOnce(Vec<T>) -> Vec<T>,
    {
        self.seek(start);
        let old = self.remove_to_mark(end);
        let new = func(old);
        self.insert_multi(new);
    }

    /// Check items with `func` until it matches (returns `Some`).  Returns the result of `func` on
    /// the matched item, and leaves the cursor just before the item itself.
    pub fn advance_until_match<F, R>(&mut self, func: F) -> Option<R>
    where
        F: FnMut(&T) -> Option<R>,
    {
        let (n, result) = if let Some((n, r)) = find_matching_result_rev(&self.right, func) {
            (n, Some(r))
        } else {
            (self.right.len(), None)
        };

        self.advance_by(n);
        result
    }

    #[allow(dead_code)] // Helper function for debugging
    pub fn debug(&self) {
        let pos = self.left.len();
        let len = self.left.len() + self.right.len();
        info!("cursor debug: at position {} / {}", pos, len);

        let mut depth = 0;
        for m in &self.left_marks {
            depth += m.depth;
            info!(
                "  mark {} at (L) {} / {}",
                m.name,
                pos - depth as usize,
                len
            );
        }
        for m in &self.right_marks {
            depth += m.depth;
            info!(
                "  mark {} at (R) {} / {}",
                m.name,
                pos + depth as usize,
                len
            );
        }
    }
}

fn find_mark_depth(marks: &[MarkData], m: Mark) -> Option<usize> {
    let mut seen = false;
    let mut sum = 0;
    for md in marks {
        if !seen {
            if md.name == m.0 {
                sum = md.depth;
                seen = true;
            }
        } else {
            sum += md.depth;
        }
    }

    if seen {
        Some(sum)
    } else {
        None
    }
}

/// Find an element matching `pred`, and returns its offset into `buf`.
fn find_matching_rev<T, F: FnMut(&T) -> bool>(buf: &[T], mut pred: F) -> Option<usize> {
    for (i, x) in buf.iter().rev().enumerate() {
        if pred(x) {
            return Some(i);
        }
    }
    None
}

/// Find an element on which `func` matches (returns `Some`), and returns its offset into `buf` and
/// the data returned by `func`.
fn find_matching_result_rev<T, F, R>(buf: &[T], mut func: F) -> Option<(usize, R)>
where
    F: FnMut(&T) -> Option<R>,
{
    for (i, x) in buf.iter().rev().enumerate() {
        if let Some(r) = func(x) {
            return Some((i, r));
        }
    }
    None
}
