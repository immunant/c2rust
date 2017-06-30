use std::collections::HashMap;
use syntax::codemap::CodeMap;
use syntax_pos::BytePos;

use rewrite::TextRewrite;


pub fn rewrite_files(cm: &CodeMap, rewrites: &[TextRewrite]) {
    let mut by_file = HashMap::new();

    for rw in rewrites {
        let fm = cm.lookup_byte_offset(rw.old_span.lo).fm;
        let ptr = &fm as *const _;
        by_file.entry(ptr).or_insert_with(|| (Vec::new(), fm)).0.push(rw.clone());
    }

    for (_, (mut rewrites, fm)) in by_file {
        let mut buf = String::new();
        rewrites.sort_by_key(|rw| rw.old_span.lo.0);
        rewrite_range(cm, fm.start_pos, fm.end_pos, &mut rewrites, &mut |s| buf.push_str(s));
        println!(" ==== {} ====\n{}\n =========", fm.name, buf);
    }
}

#[allow(dead_code)] // Helper function for debugging
fn print_rewrite(rw: &TextRewrite, depth: usize) {
    for _ in 0 .. depth {
        print!("  ");
    }
    println!("{:?} -> {:?}", rw.old_span, rw.new_span);
    for rw in &rw.rewrites {
        print_rewrite(rw, depth + 1);
    }
}

fn rewrite_range(cm: &CodeMap,
                 start: BytePos,
                 end: BytePos,
                 rewrites: &mut [TextRewrite],
                 callback: &mut FnMut(&str)) {
    let mut cur = start;

    for rw in rewrites {
        if rw.old_span.lo != cur {
            emit_chunk(cm, cur, rw.old_span.lo, |s| callback(s));
        }
        if rw.rewrites.len() == 0 {
            emit_chunk(cm, rw.new_span.lo, rw.new_span.hi, |s| callback(s));
        } else {
            rw.rewrites.sort_by_key(|rw| rw.old_span.lo.0);
            rewrite_range(cm, rw.new_span.lo, rw.new_span.hi, &mut rw.rewrites, callback);
        }
        cur = rw.old_span.hi;
    }

    if cur != end {
        emit_chunk(cm, cur, end, |s| callback(s));
    }
}

fn emit_chunk<F: FnMut(&str)>(cm: &CodeMap,
                              lo: BytePos,
                              hi: BytePos,
                              mut callback: F) {
    let lo = cm.lookup_byte_offset(lo);
    let hi = cm.lookup_byte_offset(hi);
    let src = lo.fm.src.as_ref()
        .unwrap_or_else(|| panic!("source of file {} is not available", lo.fm.name));
    callback(&src[lo.pos.0 as usize .. hi.pos.0 as usize]);
}
