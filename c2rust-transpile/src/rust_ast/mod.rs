pub mod comment_store;
pub mod item_store;
pub mod traverse;

use syntax_pos::hygiene::SyntaxContext;
use syntax_pos::{BytePos, Span};

/// Make a new span at `pos`
pub fn pos_to_span(pos: BytePos) -> Span {
    Span::new(pos, pos, SyntaxContext::root())
}
