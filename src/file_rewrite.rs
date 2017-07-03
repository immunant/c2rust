use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::rc::Rc;
use syntax::codemap::{CodeMap, FileMap};
use syntax_pos::BytePos;

use rewrite::{TextRewrite, TextAdjust};


pub enum RewriteMode {
    InPlace,
    Alongside,
    Print,
}

pub fn rewrite_files(cm: &CodeMap, rewrites: &[TextRewrite], mode: RewriteMode) {
    rewrite_files_with(cm, rewrites, |fm, s| {
        let path = match fm.abs_path.as_ref() {
            Some(x) => x,
            None => return,
        };

        match mode {
            RewriteMode::InPlace => {
                println!("writing to {}", path);
                let mut f = File::create(path).unwrap();
                f.write_all(s.as_bytes()).unwrap();
            },
            RewriteMode::Alongside => {
                let new_path = format!("{}.new", path);
                println!("writing to {}", new_path);
                let mut f = File::create(&new_path).unwrap();
                f.write_all(s.as_bytes()).unwrap();
            },
            RewriteMode::Print => {
                println!(" ==== {} ====\n{}\n =========", fm.name, s);
            },
        }
    });
}

pub fn rewrite_files_with<F>(cm: &CodeMap, rewrites: &[TextRewrite], mut callback: F)
        where F: FnMut(Rc<FileMap>, &str) {
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
        callback(fm, &buf);
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

        match rw.adjust {
            TextAdjust::None => {},
            TextAdjust::Parenthesize => callback("("),
        }

        if rw.rewrites.len() == 0 {
            emit_chunk(cm, rw.new_span.lo, rw.new_span.hi, |s| callback(s));
        } else {
            rw.rewrites.sort_by_key(|rw| rw.old_span.lo.0);
            rewrite_range(cm, rw.new_span.lo, rw.new_span.hi, &mut rw.rewrites, callback);
        }

        match rw.adjust {
            TextAdjust::None => {},
            TextAdjust::Parenthesize => callback(")"),
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
