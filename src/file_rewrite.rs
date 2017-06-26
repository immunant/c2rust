use std::collections::HashMap;
use syntax::codemap::{CodeMap, Span};
use syntax_pos::hygiene::SyntaxContext;

pub fn rewrite_files(cm: &CodeMap, rewrites: &[(Span, String)]) {
    let mut by_file = HashMap::new();

    for &(ref sp, ref text) in rewrites {
        let lo = cm.lookup_byte_offset(sp.lo);
        let hi = cm.lookup_byte_offset(sp.hi);

        let path = match lo.fm.abs_path {
            Some(ref x) => x.clone(),
            None => {
                println!("warning: span covers non-file {:?} (new text: {:?})",
                         lo.fm.name, text);
                continue;
            },
        };

        if lo.fm.abs_path != hi.fm.abs_path {
            println!("warning: span covers multiple files: {:?} != {:?} (new text: {:?})",
                     lo.fm.abs_path, hi.fm.abs_path, text);
            continue;
        }

        if sp.ctxt != SyntaxContext::empty() {
            println!("warning: can't apply rewrite inside macro definition (new text: {:?})",
                     text);
            continue;
        }

        by_file.entry(path).or_insert_with(|| (Vec::new(), lo.fm.clone()))
               .0.push((lo.pos.0 as usize, hi.pos.0 as usize, text));
    }

    for (path, (mut rewrites, fm)) in by_file {
        rewrites.sort_by_key(|x| x.0);

        let mut src_pos = 0;
        let orig = match fm.src {
            Some(ref x) => &**x,
            None => {
                println!("warning: missing source code for file {:?}", path);
                continue;
            },
        };
        let mut result = String::with_capacity(orig.len());

        println!("applying {} rewrites to {}",
                 rewrites.len(), path);
        for (lo, hi, text) in rewrites {
            result.push_str(&orig[src_pos .. lo]);
            result.push_str(&text);
            src_pos = hi;
        }
        result.push_str(&orig[src_pos..]);

        println!("result text:\n{}", result);
    }
}
