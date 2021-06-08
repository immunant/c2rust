pub mod comment_store;
pub mod item_store;
pub mod traverse;

use syntax_pos::{BytePos, Span};
use syntax_pos::hygiene::SyntaxContext;

/// Make a new span at `pos`
pub fn pos_to_span(pos: BytePos) -> Span {
    Span::new(pos, pos, SyntaxContext::root())
}
