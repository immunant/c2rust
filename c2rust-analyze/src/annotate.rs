use rustc_middle::ty::TyCtxt;
use rustc_span::{FileName, Span};
use std::collections::HashMap;
use std::fmt::Display;

pub struct AnnotationBuffer<'tcx> {
    tcx: TyCtxt<'tcx>,
    /// Map from `file_idx` to a list of annotations as `(line_number, text)` pairs.
    m: HashMap<usize, Vec<(usize, String)>>,
}

impl<'tcx> AnnotationBuffer<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> AnnotationBuffer<'tcx> {
        AnnotationBuffer {
            tcx,
            m: HashMap::new(),
        }
    }

    pub fn _clear(&mut self) {
        self.m.clear();
    }

    pub fn emit(&mut self, span: Span, msg: impl Display) {
        let sm = self.tcx.sess.source_map();

        let span = span.source_callsite();
        let pos = span.lo();
        let file_idx = sm.lookup_source_file_idx(pos);
        let sf = &sm.files()[file_idx];
        let line = sf.lookup_line(pos).unwrap_or(0);

        let src = sm
            .span_to_snippet(span)
            .unwrap_or_else(|_| "<error>".into());
        let src = src.split_ascii_whitespace().collect::<Vec<_>>().join(" ");
        let (src1, src2, src3) = if src.len() > 20 {
            (&src[..15], " ... ", &src[src.len() - 5..])
        } else {
            (&src[..], "", "")
        };
        self.m.entry(file_idx).or_insert_with(Vec::new).push((
            line,
            format!("{}: {}{}{}: {}", line + 1, src1, src2, src3, msg),
        ));
    }

    pub fn finish(self) -> HashMap<FileName, Vec<(usize, String)>> {
        let mut m = HashMap::new();
        let sm = self.tcx.sess.source_map();
        for (file_idx, v) in self.m {
            let sf = &sm.files()[file_idx];
            let old = m.insert(sf.name.clone(), v);
            assert!(
                old.is_none(),
                "found multiple SourceFiles named {:?}",
                sf.name
            );
        }
        m
    }
}
