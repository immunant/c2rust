use rustc_middle::mir::Body;
use crate::context::{PermissionSet, PointerId, AnalysisCtxt};

mod type_check;


#[derive(Clone, Debug)]
struct Edge {
    /// For each bit in `mask`, a positive edge sets that bit in `dest` if it's set in `source`; a
    /// negative edge clears the bit in `dest` if it's cleared in `source`.
    positive: bool,
    source: PointerId,
    dest: PointerId,
    mask: PermissionSet,
}

#[derive(Clone, Debug)]
struct Fixed {
    /// For each bit in `mask`, a positive fixed constraint sets the bit in `dest`; a negative
    /// constraint clear the bit in `dest` instead.
    positive: bool,
    dest: PointerId,
    mask: PermissionSet,
}

#[derive(Clone, Debug, Default)]
pub struct DataflowConstraints {
    fixed: Vec<Fixed>,
    edges: Vec<Edge>,
}

impl DataflowConstraints {
    fn add_edge(
        &mut self,
        positive: bool,
        source: PointerId,
        dest: PointerId,
        mask: PermissionSet,
    ) {
        self.edges.push(Edge { positive, source, dest, mask });
    }

    fn add_fixed(
        &mut self,
        positive: bool,
        dest: PointerId,
        mask: PermissionSet,
    ) {
        self.fixed.push(Fixed { positive, dest, mask });
    }

    /// Update the pointer permissions in `hypothesis` to satisfy these constraints.
    pub fn propagate(&self, hypothesis: &mut [PermissionSet]) -> bool {
        eprintln!("=== propagating ===");
        eprintln!("fixed:");
        for f in &self.fixed {
            eprintln!("  {:?}", f);
        }
        eprintln!("edges:");
        for e in &self.edges {
            eprintln!("  {:?}", e);
        }
        eprintln!("hypothesis:");
        for (i, p) in hypothesis.iter().enumerate() {
            eprintln!("  {}: {:?}", i, p);
        }
        match self.propagate_inner(hypothesis) {
            Ok(changed) => changed,
            Err(msg) => {
                panic!("{}", msg);
            },
        }
    }

    fn propagate_inner(&self, hypothesis: &mut [PermissionSet]) -> Result<bool, String> {
        for f in &self.fixed {
            if f.positive {
                hypothesis[f.dest.index()].insert(f.mask);
            } else {
                hypothesis[f.dest.index()].remove(f.mask);
            }
        }

        let mut changed = false;
        let mut dirty = vec![true; hypothesis.len()];
        let mut i = 0;
        loop {
            if i > hypothesis.len() + self.edges.len() {
                return Err(format!("infinite loop in dataflow edges"));
            }
            i += 1;

            let mut new_dirty = vec![false; hypothesis.len()];
            let mut any_new_dirty = false;
            for e in &self.edges {
                if !dirty[e.source.index()] {
                    continue;
                }
                let old = hypothesis[e.dest.index()];
                if e.positive {
                    hypothesis[e.dest.index()].insert(e.mask & hypothesis[e.source.index()]);
                } else {
                    hypothesis[e.dest.index()].remove(e.mask & !hypothesis[e.source.index()]);
                }
                if hypothesis[e.dest.index()] != old {
                    eprintln!("changed {:?}: {:?} => {:?}", e.dest, old, hypothesis[e.dest.index()]);
                    new_dirty[e.dest.index()] = true;
                    any_new_dirty = true;
                    changed = true;
                }
            }

            if !any_new_dirty {
                break;
            }
            dirty = new_dirty;
        }

        for f in &self.fixed {
            let ok = if f.positive {
                hypothesis[f.dest.index()].contains(f.mask)
            } else {
                (!hypothesis[f.dest.index()]).contains(f.mask)
            };
            if !ok {
                return Err(format!("fixed dataflow constraint was violated: {:?}", f));
            }
        }

        Ok(changed)
    }
}


pub fn generate_constraints<'tcx>(
    acx: &AnalysisCtxt<'tcx>,
    mir: &Body<'tcx>,
) -> DataflowConstraints {
    self::type_check::visit(acx, mir)
}
