use rustc_span::source_map::{SourceMap, Span, DUMMY_SP};

use crate::rewrite::TextRewrite;

fn empty_span(sp: Span) -> bool {
    sp.lo() == sp.hi()
}

fn equal_span_text(cm: &SourceMap, sp1: Span, sp2: Span) -> bool {
    if sp1 == sp2 {
        return true;
    }

    let lo1 = cm.lookup_byte_offset(sp1.lo());
    let hi1 = cm.lookup_byte_offset(sp1.hi());
    let src1 = lo1
        .sf
        .src
        .as_ref()
        .unwrap_or_else(|| panic!("source of file {:?} is not available", lo1.sf.name));

    let lo2 = cm.lookup_byte_offset(sp2.lo());
    let hi2 = cm.lookup_byte_offset(sp2.hi());
    let src2 = lo2
        .sf
        .src
        .as_ref()
        .unwrap_or_else(|| panic!("source of file {:?} is not available", lo2.sf.name));

    src1[lo1.pos.0 as usize..hi1.pos.0 as usize] == src2[lo2.pos.0 as usize..hi2.pos.0 as usize]
}

/// Clean up a list of rewrites, sorting them and trying to remove all overlapping rewrites without
/// affecting the meaning of the rewrite list.
pub fn cleanup_rewrites(cm: &SourceMap, rws: Vec<TextRewrite>) -> Vec<TextRewrite> {
    let mut rws = rws;
    // Sort by start position ascending, then by end position descending.  This way, in case of a
    // pair of overlapping rewrites with the same start position, we see the longest one first.
    rws.sort_by_key(|rw| (rw.old_span.lo().0, rw.old_span.hi().0));

    let mut new_rws: Vec<TextRewrite> = Vec::with_capacity(rws.len());

    for mut rw in rws {
        if rw.old_span == DUMMY_SP {
            // This should only happen when dummy content gets deleted, in which case it is a
            // no-op.
            assert!(
                rw.new_span == DUMMY_SP,
                "tried to insert text at dummy location"
            );
            continue;
        }

        rw.rewrites = cleanup_rewrites(cm, rw.rewrites);

        if new_rws
            .last()
            .map_or(true, |prev| prev.old_span.hi().0 <= rw.old_span.lo().0)
        {
            // This rewrite doesn't overlap the previous rewrite, so we can keep it.
            //
            // Note this covers the case of multiple insertions at the same point (rewriting at
            // 10..10 and then 10..10 again, for example).
            new_rws.push(rw);
            continue;
        }

        // This rewrite *does* overlap the previous rewrite.  That's not allowed in `new_rws`, so
        // we're either going to discard it or panic.

        let prev = new_rws.last().unwrap();

        if rw.old_span.lo().0 <= prev.old_span.hi().0
            && empty_span(rw.new_span)
            && empty_span(prev.new_span)
        {
            rw.old_span = rw.old_span.with_lo(prev.old_span.hi());
            new_rws.push(rw);
            continue;
        }

        // If the two rewrites have identical old_span, new text, sub-rewrites, and adjustment,
        // then we can drop the second one.  But note that even when new_span differs, the new text
        // may be the same.
        if rw.old_span == prev.old_span
            && equal_span_text(cm, rw.new_span, prev.new_span)
            && rw.rewrites == prev.rewrites
            && rw.adjust == prev.adjust
        {
            continue;
        }

        panic!("conflicting rewrites:\nprev = {:#?}\ncur = {:#?}", prev, rw);
    }

    new_rws
}
