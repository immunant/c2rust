//! Code for applying `TextRewrite`s to the actual source files.
use diff;
use log::{info, warn};
use rustc_span::source_map::{SourceFile, SourceMap};
use rustc_span::{BytePos, FileName};
use std::collections::{HashMap, VecDeque};
use std::io;

use crate::file_io::FileIO;
use crate::rewrite::cleanup::cleanup_rewrites;
use crate::rewrite::{TextAdjust, TextRewrite};

/// Apply a sequence of rewrites to the source code, handling the results by passing the new text
/// to `callback` along with the `SourceFile` describing the original source file.
pub fn rewrite_files_with(cm: &SourceMap, rw: &TextRewrite, io: &dyn FileIO) -> io::Result<()> {
    let mut by_file = HashMap::new();

    for rw in &rw.rewrites {
        let sf = cm.lookup_byte_offset(rw.old_span.lo()).sf;
        let ptr = (&sf as &SourceFile) as *const _;
        by_file
            .entry(ptr)
            .or_insert_with(|| (Vec::new(), Vec::new(), sf))
            .0
            .push(rw.clone());
    }

    for &(span, id) in &rw.nodes {
        let sf = cm.lookup_byte_offset(span.lo()).sf;
        let ptr = (&sf as &SourceFile) as *const _;
        by_file
            .entry(ptr)
            .or_insert_with(|| (Vec::new(), Vec::new(), sf))
            .1
            .push((span, id));
    }

    for (_, (rewrites, nodes, sf)) in by_file {
        let path = match sf.name {
            FileName::Real(ref path) => path,
            _ => {
                warn!("can't rewrite virtual file {:?}", sf.name);
                continue;
            }
        };
        let path = match path.local_path() {
            Some(path) => path,
            None => continue,
        };

        // TODO: do something with nodes
        io.save_rewrites(cm, &sf, &rewrites, &nodes)?;
        let mut buf = String::new();
        let rewrites = cleanup_rewrites(cm, rewrites);
        rewrite_range(cm, sf.start_pos, sf.end_pos, &rewrites, &mut |s| {
            buf.push_str(s)
        });
        io.write_file(path, &buf)?;
    }

    io.end_rewrite(cm)?;

    Ok(())
}

#[allow(dead_code)] // Helper function for debugging
fn print_rewrite(rw: &TextRewrite, depth: usize) {
    for _ in 0..depth {
        print!("  ");
    }
    info!("{:?} -> {:?}", rw.old_span, rw.new_span);
    for rw in &rw.rewrites {
        print_rewrite(rw, depth + 1);
    }
}

#[allow(dead_code)] // Helper function for debugging
fn print_rewrites(rws: &[TextRewrite]) {
    info!("{} rewrites:", rws.len());
    for rw in rws {
        info!(
            "    {:?} -> {:?} (+{} children)",
            rw.old_span,
            rw.new_span,
            rw.rewrites.len()
        );
    }
}

/// Apply a sequence of rewrites to the source text between source_map positions `start` and `end`.
/// Runs `callback` on each contiguous block of text in the rewritten version.
///
/// All rewrites must be in order, and must lie between `start` and `end`.  Otherwise a panic may
/// occur.
fn rewrite_range(
    cm: &SourceMap,
    start: BytePos,
    end: BytePos,
    rewrites: &[TextRewrite],
    callback: &mut dyn FnMut(&str),
) {
    let mut cur = start;

    for rw in rewrites {
        if rw.old_span.lo() != cur {
            emit_chunk(cm, cur, rw.old_span.lo(), |s| callback(s));
        }

        match rw.adjust {
            TextAdjust::None => {}
            TextAdjust::Parenthesize => callback("("),
        }

        if rw.rewrites.is_empty() {
            emit_chunk(cm, rw.new_span.lo(), rw.new_span.hi(), |s| callback(s));
        } else {
            rewrite_range(
                cm,
                rw.new_span.lo(),
                rw.new_span.hi(),
                &rw.rewrites,
                callback,
            );
        }

        match rw.adjust {
            TextAdjust::None => {}
            TextAdjust::Parenthesize => callback(")"),
        }

        cur = rw.old_span.hi();
    }

    if cur != end {
        emit_chunk(cm, cur, end, |s| callback(s));
    }
}

/// Runs `callback` on the source text between `lo` and `hi`.
fn emit_chunk<F: FnMut(&str)>(cm: &SourceMap, lo: BytePos, hi: BytePos, mut callback: F) {
    let lo = cm.lookup_byte_offset(lo);
    let hi = cm.lookup_byte_offset(hi);
    let src = lo
        .sf
        .src
        .as_ref()
        .unwrap_or_else(|| panic!("source of file {:?} is not available", lo.sf.name));
    callback(&src[lo.pos.0 as usize..hi.pos.0 as usize]);
}

/// Print a unified diff between lines of `s1` and lines of `s2`.
pub fn print_diff(s1: &str, s2: &str) {
    enum State {
        /// We're not in a hunk, just keeping `buf` populated with `CONTEXT` lines of history.
        History,
        /// We're inside a hunk containing at least one changed line.
        Hunk {
            /// Number of unchanged lines we need to see to end this hunk.
            unchanged_limit: usize,
            l_start: usize,
            r_start: usize,
        },
    }

    const CONTEXT: usize = 3;

    let mut buf = VecDeque::new();
    let mut state = State::History;

    let mut l_line = 1;
    let mut r_line = 1;

    for r in diff::lines(s1, s2) {
        let changed = match r {
            diff::Result::Both(l, r) => l != r,
            _ => true,
        };

        // We need to update l/r_line before we move `r`, but after we move it, we may need access
        // to the old l/r_line values to initialize state.l/r_start.
        let (l_line_old, r_line_old) = (l_line, r_line);

        match r {
            diff::Result::Left(..) => {
                l_line += 1;
            }
            diff::Result::Right(..) => {
                r_line += 1;
            }
            diff::Result::Both(..) => {
                l_line += 1;
                r_line += 1;
            }
        }

        buf.push_back(r);

        if !changed {
            match state {
                State::History => {
                    while buf.len() > CONTEXT {
                        buf.pop_front();
                    }
                }
                State::Hunk {
                    unchanged_limit,
                    l_start,
                    r_start,
                } => {
                    if unchanged_limit == 1 {
                        // End of the hunk
                        let end = buf.len() - CONTEXT;
                        let suffix = buf.split_off(end);
                        print_hunk(&buf, l_start, r_start);
                        buf = suffix;
                        state = State::History;
                    } else {
                        state = State::Hunk {
                            unchanged_limit: unchanged_limit - 1,
                            l_start,
                            r_start,
                        };
                    }
                }
            }
        } else {
            match state {
                State::History => {
                    state = State::Hunk {
                        unchanged_limit: 2 * CONTEXT,
                        // Adjust start lines for context already stored in `buf`.
                        l_start: l_line_old - (buf.len() - 1),
                        r_start: r_line_old - (buf.len() - 1),
                    };
                }
                State::Hunk {
                    l_start, r_start, ..
                } => {
                    state = State::Hunk {
                        unchanged_limit: 2 * CONTEXT,
                        l_start,
                        r_start,
                    };
                }
            }
        }
    }

    match state {
        State::Hunk {
            unchanged_limit,
            l_start,
            r_start,
        } => {
            if unchanged_limit < CONTEXT {
                let end = buf.len() - (CONTEXT - unchanged_limit);
                buf.truncate(end);
            }
            print_hunk(&buf, l_start, r_start);
        }
        _ => {}
    }
}

/// Print a single diff hunk, starting at line `l_start` in the left file and `r_start` in the
/// right file.
fn print_hunk(buf: &VecDeque<diff::Result<&str>>, l_start: usize, r_start: usize) {
    let l_size = buf
        .iter()
        .filter(|r| match r {
            diff::Result::Right(_) => false,
            _ => true,
        })
        .count();

    let r_size = buf
        .iter()
        .filter(|r| match r {
            diff::Result::Left(_) => false,
            _ => true,
        })
        .count();

    println!("@@ -{},{} +{},{} @@", l_start, l_size, r_start, r_size);

    // Print all "left" lines immediately.  Keep all "right" lines and print them just before the
    // next unchanged line.  This way we get the usual output, with separate old and new blocks:
    //   unchanged
    //  -old1
    //  -old2
    //  +new1
    //  +new2
    //   unchanged
    let mut right_buf = Vec::new();
    for r in buf {
        match r {
            diff::Result::Left(s) => {
                println!("-{}", s);
            }
            diff::Result::Right(s) => {
                right_buf.push(s);
            }
            diff::Result::Both(s1, s2) => {
                if s1 != s2 {
                    println!("-{}", s1);
                    right_buf.push(s2);
                } else {
                    for s in right_buf.drain(..) {
                        println!("+{}", s);
                    }
                    println!(" {}", s1);
                }
            }
        }
    }
}
