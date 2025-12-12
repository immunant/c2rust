use proc_macro2::{Delimiter, Spacing, Span, TokenStream, TokenTree};
use std::collections::BTreeSet;

pub struct FlatTokens {
    stack: Vec<(proc_macro2::token_stream::IntoIter, Delimiter, Span)>,
}

impl FlatTokens {
    pub fn new(ts: TokenStream) -> FlatTokens {
        FlatTokens {
            stack: vec![(ts.into_iter(), Delimiter::None, Span::call_site())],
        }
    }
}

impl Iterator for FlatTokens {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        while let Some(&mut (ref mut it, delim, span_close)) = self.stack.last_mut() {
            match it.next() {
                Some(TokenTree::Group(g)) => {
                    // Return the open delimiter and continue with the contents of the group.
                    self.stack
                        .push((g.stream().into_iter(), g.delimiter(), g.span_close()));
                    let open_ch = match g.delimiter() {
                        Delimiter::Parenthesis => '(',
                        Delimiter::Bracket => '[',
                        Delimiter::Brace => '{',
                        // Skip over `Delimiter::None`, and `continue` to try the next available
                        // token.
                        Delimiter::None => continue,
                    };
                    let range = g.span_open().byte_range();
                    return Some(Token {
                        text: open_ch.to_string().into(),
                        spacing: Spacing::Alone,
                        span: (range.start, range.end),
                    });
                }
                Some(tt @ TokenTree::Ident(_))
                | Some(tt @ TokenTree::Punct(_))
                | Some(tt @ TokenTree::Literal(_)) => return Some(Token::from_token_tree(tt)),
                None => {
                    // Pop the now-empty group and return the close delimiter.
                    self.stack.pop();
                    let close_ch = match delim {
                        Delimiter::Parenthesis => ')',
                        Delimiter::Bracket => ']',
                        Delimiter::Brace => '}',
                        // Skip over `Delimiter::None`, and `continue` to try the next available
                        // token.
                        Delimiter::None => continue,
                    };
                    let range = span_close.byte_range();
                    return Some(Token {
                        text: close_ch.to_string().into(),
                        spacing: Spacing::Alone,
                        span: (range.start, range.end),
                    });
                }
            }
        }
        None
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Token {
    pub text: Box<str>,
    /// Spacing, for punctuation tokens.  This is `Spacing::Alone` for all non-`Punct` tokens.
    pub spacing: Spacing,
    pub span: (usize, usize),
}

impl Token {
    pub fn from_token_tree(tt: TokenTree) -> Token {
        let text = tt.to_string().into_boxed_str();
        let spacing = match tt {
            TokenTree::Punct(ref p) => p.spacing(),
            _ => Spacing::Alone,
        };
        let range = tt.span().byte_range();
        let start = range.start;
        let end = range.end;
        Token {
            text,
            spacing,
            span: (start, end),
        }
    }
}

pub struct TokenIndex<'a> {
    tokens: &'a [Token],
    /// Multi-map mapping `(start, end)` to indexes in `tokens` that have that span.
    index: BTreeSet<((usize, usize), usize)>,
}

impl<'a> TokenIndex<'a> {
    pub fn new(tokens: &'a [Token]) -> TokenIndex<'a> {
        let mut index = BTreeSet::new();
        for (i, t) in tokens.iter().enumerate() {
            index.insert((t.span, i));
        }
        TokenIndex { tokens, index }
    }

    pub fn find<'b>(&'b self, t: &Token) -> Option<usize> {
        let (start, end) = t.span;
        let lo = ((start, end), 0);
        let hi = ((start, end), usize::MAX);
        let mut found = None;
        for &(_, i) in self.index.range(lo..=hi) {
            if self.tokens[i] == *t {
                if found.is_none() {
                    found = Some(i);
                } else {
                    // Found multiple identical tokens.  This is ambiguous.
                    return None;
                }
            }
        }
        found
    }
}

pub struct OutputBuffer {
    s: String,
    /// Whether the most recently emitted chunk had `Spacing::Joint`.
    prev_was_joint: bool,
    /// Index in `s` of the start of the most recent line.
    prev_bol: usize,
}

impl OutputBuffer {
    pub fn new() -> OutputBuffer {
        OutputBuffer {
            s: String::new(),
            prev_was_joint: true,
            prev_bol: 0,
        }
    }

    /// Emit a token or a group of tokens.
    ///
    /// This assumes that the start of `chunk` is the start of a token (not whitespace or a non-doc
    /// comment) and that the end of `chunk` is the end of a token.
    pub fn emit(&mut self, chunk: &str, spacing: Spacing) {
        let cur_line = &self.s[self.prev_bol..];
        if self.prev_was_joint {
            // No whitespace is allowed between a `Joint` token and the subsequent token.
        } else {
            if cur_line.contains("//") {
                // Note this can throw false positives, such as if `//` appears inside a string
                // literal.  But it's legal to replace any `' '` with `'\n'`; `rustfmt` will fix it
                // if needed.
                self.s.push('\n');
                self.prev_bol = self.s.len();
            } else {
                self.s.push(' ');
            }
        }

        // If `chunk` contains a newline, update the beginning-of-line position.
        if let Some(i) = chunk.bytes().rposition(|b| b == b'\n') {
            self.prev_bol = self.s.len() + i + 1;
        }

        self.s.push_str(chunk);
        self.prev_was_joint = matches!(spacing, Spacing::Joint);
    }

    pub fn finish(self) -> String {
        self.s
    }
}

pub fn render_output(
    orig: &str,
    orig_tokens: &[Token],
    ti: &TokenIndex,
    ts: TokenStream,
    buf: &mut OutputBuffer,
) {
    if let Some(t) = orig_tokens.get(0) {
        let (start_pos, _) = t.span;
        buf.emit(&orig[0..start_pos], Spacing::Joint);
    }

    struct Run {
        /// Byte offset of the start of the first token in the run.
        start_pos: usize,
        /// Index in `orig_tokens` of the last token in the run.
        end_idx: usize,
    }
    let mut current_run: Option<Run> = None;
    for t in FlatTokens::new(ts) {
        // Try to continue the current run.
        if let Some(ref mut run) = current_run {
            if orig_tokens.get(run.end_idx + 1) == Some(&t) {
                run.end_idx += 1;
                continue;
            } else {
                // End the current run.
                let end_token = &orig_tokens[run.end_idx];
                let start_pos = run.start_pos;
                let (_, end_pos) = end_token.span;
                buf.emit(&orig[start_pos..end_pos], end_token.spacing);
                current_run = None;
            }
        }

        // Try to start a new run.
        debug_assert!(current_run.is_none());
        if let Some(idx) = ti.find(&t) {
            let (start_pos, _) = t.span;
            current_run = Some(Run {
                start_pos,
                end_idx: idx,
            });
            continue;
        }

        // This token is not part of a run.  Emit it directly.
        buf.emit(&t.text, t.spacing);
    }

    if let Some(run) = current_run {
        let end_token = &orig_tokens[run.end_idx];
        let start_pos = run.start_pos;
        let (_, end_pos) = end_token.span;
        buf.emit(&orig[start_pos..end_pos], end_token.spacing);
    }

    if let Some(t) = orig_tokens.last() {
        let (_, end_pos) = t.span;
        buf.emit(&orig[end_pos..orig.len()], Spacing::Joint);
    }
}
