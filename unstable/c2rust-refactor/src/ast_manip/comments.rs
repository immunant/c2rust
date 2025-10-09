use rustc_ast::*;
use std::collections::HashMap;
use std::iter::Peekable;
use std::ops::Index;
use std::slice;
// use rustc_ast::util::comments::Comment as LexComment;
use rustc_ast::visit::*;
use rustc_session::parse::ParseSess;
use rustc_span::source_map::{SourceMap, Span};
use rustc_span::{BytePos, CharPos, FileName, Pos};

use crate::ast_manip::Visit;

pub use rustc_ast::util::comments::{Comment, CommentStyle};

#[derive(Default)]
pub struct CommentMap(HashMap<NodeId, Vec<Comment>>);

impl CommentMap {
    pub fn insert(&mut self, id: NodeId, comment: Comment) {
        let comment = Comment {
            style: comment.style,
            lines: comment.lines,
            pos: comment.pos,
        };
        self.0.entry(id).or_default().push(comment);
    }

    pub fn get(&self, k: &NodeId) -> Option<&[Comment]> {
        self.0.get(k).map(Vec::as_slice)
    }
}

impl Index<&NodeId> for CommentMap {
    type Output = [Comment];

    fn index(&self, key: &NodeId) -> &[Comment] {
        self.0.index(key)
    }
}

pub fn collect_comments<T>(ast: &T, comments: &[Comment]) -> CommentMap
where
    T: Visit,
{
    let mut collector = CommentCollector {
        comment_map: CommentMap::default(),
        cur_comment: comments.iter().peekable(),
    };

    ast.visit(&mut collector);

    collector.comment_map
}

struct CommentCollector<'a> {
    comment_map: CommentMap,

    cur_comment: Peekable<slice::Iter<'a, Comment>>,
}

impl<'a> CommentCollector<'a> {
    fn next_comment(&mut self) -> Option<&Comment> {
        while let Some(comment) = self.cur_comment.peek() {
            match comment.style {
                CommentStyle::Isolated | CommentStyle::Trailing => {
                    return Some(comment);
                }

                CommentStyle::Mixed | CommentStyle::BlankLine => {
                    self.cur_comment.next();
                }
            }
        }
        None
    }

    fn consume_comment(&mut self) -> Comment {
        self.cur_comment.next().unwrap().clone()
    }

    fn check_comment(&mut self, id: NodeId, span: Span) {
        while let Some(comment) = self.next_comment() {
            match comment.style {
                CommentStyle::Isolated => {
                    if comment.pos < span.lo() {
                        let comment = self.consume_comment();
                        self.comment_map.insert(id, comment);
                        continue;
                    }
                }
                CommentStyle::Trailing => {
                    if comment.pos >= span.hi() {
                        let comment = self.consume_comment();
                        self.comment_map.insert(id, comment);
                        continue;
                    }
                }
                _ => {}
            }

            break;
        }
    }
}

macro_rules! check_comment {
    ($visit_fn:ident, $NodeTy:ty, $walk_fn:ident) => {
        fn $visit_fn(&mut self, x: &'a $NodeTy) {
            self.check_comment(x.id, x.span);
            $walk_fn(self, x);
        }
    };
}

impl<'a> Visitor<'a> for CommentCollector<'a> {
    check_comment!(visit_item, Item, walk_item);
    check_comment!(visit_stmt, Stmt, walk_stmt);
    check_comment!(visit_expr, Expr, walk_expr);
    check_comment!(visit_foreign_item, ForeignItem, walk_foreign_item);
    fn visit_mac_call(&mut self, mac: &'a MacCall) {
        walk_mac(self, mac);
    }
}

// From librustc_ast::util::comments
/// Returns `None` if the first `col` chars of `s` contain a non-whitespace char.
/// Otherwise returns `Some(k)` where `k` is first char offset after that leading
/// whitespace. Note that `k` may be outside bounds of `s`.
fn all_whitespace(s: &str, col: CharPos) -> Option<usize> {
    let mut idx = 0;
    for (i, ch) in s.char_indices().take(col.to_usize()) {
        if !ch.is_whitespace() {
            return None;
        }
        idx = i + ch.len_utf8();
    }
    Some(idx)
}

// From librustc_ast::util::comments
fn trim_whitespace_prefix(s: &str, col: CharPos) -> &str {
    let len = s.len();
    match all_whitespace(&s, col) {
        Some(col) => {
            if col < len {
                &s[col..]
            } else {
                ""
            }
        }
        None => s,
    }
}

// From librustc_ast::util::comments
fn split_block_comment_into_lines(text: &str, col: CharPos) -> Vec<String> {
    let mut res: Vec<String> = vec![];
    let mut lines = text.lines();
    // just push the first line
    res.extend(lines.next().map(|it| it.to_string()));
    // for other lines, strip common whitespace prefix
    for line in lines {
        res.push(trim_whitespace_prefix(line, col).to_string())
    }
    res
}

// From librustc_ast::util::comments
pub fn gather_comments(sess: &ParseSess, path: FileName, src: String) -> Vec<Comment> {
    use rustc_ast::util::comments::CommentStyle::*;

    let cm = SourceMap::new(sess.source_map().path_mapping().clone());
    let source_file = cm.new_source_file(path, src);
    let text = (*source_file.src.as_ref().unwrap()).clone();

    let text: &str = text.as_str();
    let start_bpos = source_file.start_pos;
    let mut pos = 0;
    let mut comments: Vec<Comment> = Vec::new();
    let mut code_to_the_left = false;

    if let Some(shebang_len) = rustc_lexer::strip_shebang(text) {
        comments.push(Comment {
            style: Isolated,
            lines: vec![text[..shebang_len].to_string()],
            pos: start_bpos,
        });
        pos += shebang_len;
    }

    for token in rustc_lexer::tokenize(&text[pos..]) {
        let token_text = &text[pos..pos + token.len as usize];
        match token.kind {
            rustc_lexer::TokenKind::Whitespace => {
                if let Some(mut idx) = token_text.find('\n') {
                    code_to_the_left = false;
                    while let Some(next_newline) = &token_text[idx + 1..].find('\n') {
                        idx = idx + 1 + next_newline;
                        comments.push(Comment {
                            style: BlankLine,
                            lines: vec![],
                            pos: start_bpos + BytePos((pos + idx) as u32),
                        });
                    }
                }
            }
            rustc_lexer::TokenKind::BlockComment { doc_style, .. } => {
                if doc_style.is_none() {
                    let code_to_the_right = match text[pos + token.len as usize..].chars().next() {
                        Some('\r') | Some('\n') => false,
                        _ => true,
                    };
                    let style = match (code_to_the_left, code_to_the_right) {
                        (true, true) | (false, true) => Mixed,
                        (false, false) => Isolated,
                        (true, false) => Trailing,
                    };

                    // Count the number of chars since the start of the line by rescanning.
                    let pos_in_file = start_bpos + BytePos(pos as u32);
                    let line_begin_in_file = source_file.line_begin_pos(pos_in_file);
                    let line_begin_pos = (line_begin_in_file - start_bpos).to_usize();
                    let col = CharPos(text[line_begin_pos..pos].chars().count());

                    let lines = split_block_comment_into_lines(token_text, col);
                    comments.push(Comment {
                        style,
                        lines,
                        pos: pos_in_file,
                    })
                }
            }
            rustc_lexer::TokenKind::LineComment { doc_style } => {
                if doc_style.is_none() {
                    comments.push(Comment {
                        style: if code_to_the_left { Trailing } else { Isolated },
                        lines: vec![token_text.to_string()],
                        pos: start_bpos + BytePos(pos as u32),
                    })
                }
            }
            _ => {
                code_to_the_left = true;
            }
        }
        pos += token.len as usize;
    }

    comments
}
