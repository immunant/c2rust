use std::collections::HashMap;
use std::iter::Peekable;
use std::ops::Index;
use std::slice;
use syntax::ast::*;
use syntax::parse::lexer::comments::Comment as LexComment;
use syntax::source_map::{BytePos, Span};
use syntax::visit::*;

use crate::ast_manip::Visit;

pub use syntax::parse::lexer::comments::CommentStyle;

/// This is equivalent to syntax::parse::lexer::comments::Comment, but we need
/// to implement PartialEq and Debug for it
#[derive(Clone, PartialEq, Debug)]
pub struct Comment {
    pub style: CommentStyle,
    pub lines: Vec<String>,
    pub pos: BytePos,
}

#[derive(Default)]
pub struct CommentMap(HashMap<NodeId, Vec<Comment>>);

impl CommentMap {
    pub fn insert(&mut self, id: NodeId, comment: LexComment) {
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

pub fn collect_comments<T>(ast: &T, comments: &[LexComment]) -> CommentMap
    where T: Visit
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

    cur_comment: Peekable<slice::Iter<'a, LexComment>>,
}

impl<'a> CommentCollector<'a> {
    fn next_comment(&mut self) -> Option<&LexComment> {
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

    fn consume_comment(&mut self) -> LexComment {
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
    }
}

impl<'a> Visitor<'a> for CommentCollector<'a> {
    check_comment!(visit_item, Item, walk_item);
    check_comment!(visit_stmt, Stmt, walk_stmt);
    check_comment!(visit_expr, Expr, walk_expr);
    check_comment!(visit_foreign_item, ForeignItem, walk_foreign_item);
    fn visit_mac(&mut self, mac: &'a Mac) {
        walk_mac(self, mac);
    }
}
