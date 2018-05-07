use syntax_pos::BytePos;
use syntax_pos::hygiene::SyntaxContext;
use syntax::codemap::{DUMMY_SP, Span};
use syntax::parse::lexer::comments;

///
pub struct CommentStore {
    output_comments: Vec<comments::Comment>,
    span_source: u32,
}

impl CommentStore {

    pub fn new() -> Self {
        CommentStore {
            output_comments: vec![],
            span_source: 0,
        }
    }

    /// Nuke the comment context and get back the accumulated (and ordered) `libsyntax` comments.
    pub fn into_comments(self) -> Vec<comments::Comment> {
        self.output_comments
    }

    /// Add a `Comment` at the current position, then return the `Span` that should be given to
    /// something we want associated with this comment.
    pub fn add_comment(&mut self, lines: Vec<String>) -> Span {
        let lines: Vec<String> = lines
            .into_iter()
            .map(|mut comment| {
                if comment.starts_with("//!") || comment.starts_with("///") ||
                    comment.starts_with("/**") || comment.starts_with("/*!") {
                    comment.insert(2,' ');
                }
                comment
            })
            .collect();

        if lines.is_empty() {
            DUMMY_SP
        } else {
            self.span_source += 1;
            self.output_comments.push(comments::Comment {
                style: comments::CommentStyle::Isolated,
                lines: lines,
                // .lines().map(String::from).collect(),
                pos: BytePos(self.span_source),
            });

            self.span_source += 1;
            Span::new(BytePos(self.span_source), BytePos(self.span_source), SyntaxContext::empty())
        }
    }
}