use std::collections::HashMap;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{BasicBlock, Local, Location, StatementKind, TerminatorKind, Rvalue, Place, Body};
use log::error;

/// Table for looking up the most recent write to a `Local` prior to a particular MIR statement.
pub struct RecentWrites {
    blocks: IndexVec<BasicBlock, BlockWrites>,
    /// For each local, whether its address is taken anywhere within the current function.
    addr_taken: IndexVec<Local, bool>,
}

#[derive(Clone, Debug)]
struct BlockWrites {
    /// For each local, the most recent write before entry to this block.
    entry: IndexVec<Local, Written>,
    /// For each local, a list of statement indices where the local was written within this block.
    writes: HashMap<Local, Vec<Write>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
struct Write {
    statement_index: usize,
    /// Whether this is a partial write, which only modifies part of the local.
    partial: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum Written {
    /// The local hasn't been written yet.
    Never,
    /// The last write to the local was at this `Location`.
    At(Location),
    /// The last write to the local could be one of several distinct locations.
    Multiple,
}

impl Written {
    pub fn join(self, other: Written) -> Written {
        use self::Written::*;
        match (self, other) {
            (x, Never) | (Never, x) => x,
            (_, Multiple) | (Multiple, _) => Multiple,
            (At(loc1), At(loc2)) => {
                if loc1 == loc2 {
                    At(loc1)
                } else {
                    Multiple
                }
            },
        }
    }
}

impl BlockWrites {
    /// Get the `Location` of the most recent write to `l` prior to `loc`.  `loc` must be a
    /// location within the block described by this `BlockWrites`.  Returns `None` if there are two
    /// or more locations that may have written to `l`, or if `l` is uninitialized.
    pub fn get_write_before_ignoring_addr_taken(
        &self,
        loc: Location,
        l: Local,
    ) -> Option<Location> {
        if let Some(writes) = self.writes.get(&l) {
            // `l` was written at some point within this block.  Find the first `statement_index`
            // in `writes` that's strictly less than `loc.statement_index`.
            let write = if loc.statement_index == 0 {
                None
            } else if writes.len() < 10 {
                writes.iter().rfind(|&write| {
                    write.statement_index < loc.statement_index
                })
            } else {
                let i = writes.partition_point(|write| {
                    write.statement_index < loc.statement_index
                });
                if i == 0 {
                    None
                } else {
                    Some(&writes[i - 1])
                }
            };
            if let Some(write) = write {
                if write.partial {
                    // If the local was only partially written here, then this is only one of
                    // several rewrites potentially affecting its value.
                    return None;
                } else {
                    return Some(Location {
                        block: loc.block,
                        statement_index: write.statement_index,
                    });
                }
            }
        }

        // `l` was not written between the start of the block and `loc`.  Report its most recent
        // write prior to block entry.
        match self.entry[l] {
            Written::Never => None,
            Written::At(write_loc) => Some(write_loc),
            Written::Multiple => None,
        }
    }

    fn exit_state(&self, block: BasicBlock) -> IndexVec<Local, Written> {
        let mut state = self.entry.clone();
        for (&l, writes) in self.writes.iter() {
            if let Some(&last_write) = writes.last() {
                if last_write.partial {
                    state[l] = Written::Multiple;
                } else {
                    state[l] = Written::At(Location {
                        block,
                        statement_index: last_write.statement_index,
                    });
                }
            }
        }
        state
    }
}

impl RecentWrites {
    pub fn new<'tcx>(mir: &Body<'tcx>) -> RecentWrites {
        calc_recent_writes(mir)
    }

    /// Get the `Location` of the most recent write to `l` prior to `loc`.  `loc` must be a
    /// location within the block described by this `BlockWrites`.  Returns `None` if there are two
    /// or more locations that may have written to `l`, if `l` is uninitialized, or if `l` has had
    /// its address taken.
    pub fn get_write_before(&self, loc: Location, l: Local) -> Option<Location> {
        if self.addr_taken[l] {
            return None;
        }
        self.get_write_before_ignoring_addr_taken(loc, l)
    }

    pub fn get_write_before_ignoring_addr_taken(
        &self,
        loc: Location,
        l: Local,
    ) -> Option<Location> {
        self.blocks[loc.block].get_write_before_ignoring_addr_taken(loc, l)
    }

    /// Record a write to a `Place` occurring at `loc`.
    fn record_place_written(&mut self, loc: Location, pl: Place) {
        if pl.is_indirect() {
            // A write through a pointer doesn't directly write any locals, except possibly those
            // that previously had their address taken.
            return;
        }
        // If `pl.projection` is non-empty, then only a portion of the local was modified; the rest
        // was written at some previous location.
        let partial = pl.projection.len() > 0;
        self.blocks[loc.block].writes.entry(pl.local).or_insert_with(Vec::new).push(Write {
            statement_index: loc.statement_index,
            partial,
        });
    }

    /// Record a write to part of a `Place`.
    fn record_place_written_partial(&mut self, loc: Location, pl: Place) {
        // Like `record_place_written`, but `partial` is always true.
        if pl.is_indirect() {
            return;
        }
        self.blocks[loc.block].writes.entry(pl.local).or_insert_with(Vec::new).push(Write {
            statement_index: loc.statement_index,
            partial: true,
        });
    }

    /// Record a write to a `Local` occurring at `loc`.
    fn record_local_written(&mut self, loc: Location, l: Local) {
        self.blocks[loc.block].writes.entry(l).or_insert_with(Vec::new).push(Write {
            statement_index: loc.statement_index,
            partial: false,
        });
    }

    fn record_addr_taken(&mut self, pl: Place) {
        if pl.is_indirect() {
            // This is actually a projection on an address taken earlier.
            return;
        }
        // We treat taking the address of a part of the local the same as taking the address of the
        // whole local.
        self.addr_taken[pl.local] = true;
    }
}

fn calc_recent_writes(mir: &Body) -> RecentWrites {
    let default_block_writes = BlockWrites {
        entry: IndexVec::from_elem(Written::Never, &mir.local_decls),
        writes: HashMap::new(),
    };
    let mut rw = RecentWrites {
        blocks: IndexVec::from_elem(default_block_writes, &mir.basic_blocks),
        addr_taken: IndexVec::from_elem(false, &mir.local_decls),
    };

    scan_blocks(mir, &mut rw);

    let default_state = IndexVec::from_elem(Written::Never, &mir.local_decls);
    // Cached copy of `exit_state()` for each basic block.
    //
    // This is actually left uninitialized at first; it's more efficient to initialize it during
    // the first time around the outer loop.  To support this, we initialize `needs_update` to
    // `true` for every block, so the first iteration will update `exit_states` for every block.
    let mut exit_states = IndexVec::from_elem(default_state.clone(), &mir.basic_blocks);
    // The `needs_update` for a given basic block is set if its predecessors' exit states have
    // changed since the last time the block was processed.
    let mut needs_update = IndexVec::from_elem(true, &mir.basic_blocks);
    let block_preds = mir.basic_blocks.predecessors(); 
    loop {
        let mut updated = false;
        for &bb in mir.basic_blocks.postorder().iter().rev() {
            if !needs_update[bb] {
                continue;
            }
            // Clear `needs_update` early.  If `bb` is its own successor, this flag will be set
            // again below.
            needs_update[bb] = false;

            // Recompute `entry` using predecessors' exit states.
            let mut entry = default_state.clone();
            for &pred in &block_preds[bb] {
                for (a, &b) in entry.iter_mut().zip(exit_states[pred].iter()) {
                    *a = a.join(b);
                }
            }
            rw.blocks[bb].entry = entry;

            // Recompute the exit state.
            let exit = rw.blocks[bb].exit_state(bb);
            if exit != exit_states[bb] {
                exit_states[bb] = exit;
                updated = true;

                // Since `bb`'s exit state changed, its successors should recompute their entry
                // states.
                for succ in mir.basic_blocks[bb].terminator().successors() {
                    needs_update[succ] = true;
                }
            }
        }
        if !updated {
            break;
        }
    }

    rw
}

/// Initial phase of `calc_recent_writes`: scan each block and record any writes and any address-of
/// operations found within that block.
fn scan_blocks(mir: &Body, rw: &mut RecentWrites) {
    for (block, bb_data) in mir.basic_blocks().iter_enumerated() {
        for (statement_index, stmt) in bb_data.statements.iter().enumerate() {
            let loc = Location { block, statement_index };
            match stmt.kind {
                StatementKind::Assign(ref x) => {
                    let (pl, rv) = (x.0, &x.1);
                    rw.record_place_written(loc, pl);
                    match *rv {
                        Rvalue::AddressOf(_, pl) | Rvalue::Ref(_, _, pl) =>
                            rw.record_addr_taken(pl),
                        _ => {},
                    }
                },
                StatementKind::SetDiscriminant { ref place, .. } =>
                    rw.record_place_written_partial(loc, **place),
                StatementKind::Deinit(ref pl) =>
                    rw.record_place_written_partial(loc, **pl),
                StatementKind::StorageLive(l) => rw.record_local_written(loc, l),
                StatementKind::StorageDead(l) => rw.record_local_written(loc, l),
                _ => {},
            }
        }

        // Terminator
        let statement_index = bb_data.statements.len();
        let loc = Location { block, statement_index };
        match bb_data.terminator().kind {
            TerminatorKind::Drop { place, .. } |
            TerminatorKind::DropAndReplace { place, .. } =>
                rw.record_place_written(loc, place),
            TerminatorKind::Call { destination, .. } =>
                rw.record_place_written(loc, destination),
            TerminatorKind::Yield { resume_arg, .. } =>
                rw.record_place_written(loc, resume_arg),
            TerminatorKind::InlineAsm { .. } =>
                error!("recent_writes InlineAsm NYI"),
            _ => {},
        }
    }
}
