use syntax::ast::NodeId;
use syntax::source_map::{SourceMap, Span};
use json::{self, JsonValue};

use rewrite::{TextRewrite, TextAdjust};


struct Encoder<'a> {
    sm: &'a SourceMap,
}

impl<'a> Encoder<'a> {
    fn encode_rewrite(&self, r: &TextRewrite) -> JsonValue {
        object! {
            "old_span" => self.encode_span(r.old_span),
            "new_span" => self.encode_span(r.new_span),
            "rewrites" => JsonValue::Array(
                r.rewrites.iter().map(|r| self.encode_rewrite(r)).collect()),
            "nodes" => JsonValue::Array(
                r.nodes.iter().map(|&(span, id)| self.encode_node(span, id)).collect()),
            "adjust" => self.encode_adjust(r.adjust),
        }
    }

    fn encode_span(&self, sp: Span) -> JsonValue {
        let lo = self.sm.lookup_byte_offset(sp.lo());
        let hi = self.sm.lookup_byte_offset(sp.hi());
        let src = &lo.sf.src.as_ref().unwrap()[lo.pos.0 as usize .. hi.pos.0 as usize];

        object! {
            "file" => lo.sf.name.to_string(),
            "lo" => lo.pos.0,
            "hi" => hi.pos.0,
            "src" => src,
        }
    }

    fn encode_node(&self, sp: Span, id: NodeId) -> JsonValue {
        object! {
            "span" => self.encode_span(sp),
            "id" => id.as_usize(),
        }
    }

    fn encode_adjust(&self, adj: TextAdjust) -> JsonValue {
        match adj {
            TextAdjust::None => JsonValue::Null,
            TextAdjust::Parenthesize => JsonValue::String("parenthesize".to_owned()),
        }
    }
}

pub fn encode_rewrite(sm: &SourceMap, r: &TextRewrite) -> JsonValue {
    Encoder { sm }.encode_rewrite(r)
}

pub fn encode_rewrites(sm: &SourceMap, rs: &[TextRewrite]) -> JsonValue {
    let enc = Encoder { sm };
    JsonValue::Array(rs.iter().map(|r| enc.encode_rewrite(r)).collect())
}

pub fn stringify_rewrite(sm: &SourceMap, r: &TextRewrite) -> String {
    json::stringify_pretty(encode_rewrite(sm, r), 2)
}

pub fn stringify_rewrites(sm: &SourceMap, rs: &[TextRewrite]) -> String {
    json::stringify_pretty(encode_rewrites(sm, rs), 2)
}
