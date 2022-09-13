#![feature(rustc_private)]
extern crate either;
extern crate rustc_arena;
extern crate rustc_ast;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_type_ir;

use crate::context::{
    AnalysisCtxt, AnalysisCtxtData, FlagSet, GlobalAnalysisCtxt, GlobalAssignment, LFnSig, LTy,
    LTyCtxt, LocalAssignment, PermissionSet, PointerId,
};
use crate::dataflow::DataflowConstraints;
use crate::equiv::{GlobalEquivSet, LocalEquivSet};
use crate::util::Callee;
use rustc_hir::def_id::LocalDefId;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::visit::Visitor;
use rustc_middle::mir::{BindingForm, Body, LocalDecl, LocalInfo, LocalKind, Location, Operand};
use rustc_middle::ty::{Ty, TyCtxt, TyKind, WithOptConstParam};
use rustc_span::Span;
use std::collections::{HashMap, HashSet};
use std::env;
use std::ops::{Deref, DerefMut};

mod borrowck;
mod context;
mod dataflow;
mod equiv;
mod expr_rewrite;
mod labeled_ty;
mod pointer_id;
mod type_desc;
mod util;

/// A wrapper around `T` that dynamically tracks whether it's initialized or not.  `RefCell`
/// dynamically tracks borrowing and panics if the rules are violated at run time; `MaybeUnset`
/// dynamically tracks initialization and similarly panics if the value is accessed while unset.
#[derive(Clone, Copy, Debug)]
struct MaybeUnset<T>(Option<T>);

impl<T> Default for MaybeUnset<T> {
    fn default() -> MaybeUnset<T> {
        MaybeUnset(None)
    }
}

impl<T> MaybeUnset<T> {
    pub fn set(&mut self, x: T) {
        if self.0.is_some() {
            panic!("value is already set");
        }
        self.0 = Some(x);
    }

    pub fn clear(&mut self) {
        if self.0.is_none() {
            panic!("value is already cleared");
        }
        self.0 = None;
    }

    pub fn get(&self) -> &T {
        self.0.as_ref().expect("value is not set")
    }

    pub fn get_mut(&mut self) -> &mut T {
        self.0.as_mut().expect("value is not set")
    }

    pub fn take(&mut self) -> T {
        self.0.take().expect("value is not set")
    }
}

impl<T> Deref for MaybeUnset<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.get()
    }
}

impl<T> DerefMut for MaybeUnset<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.get_mut()
    }
}

fn run(tcx: TyCtxt) {
    let mut gacx = GlobalAnalysisCtxt::new(tcx);
    let mut func_info = HashMap::new();

    /// Local information, specific to a single function.  Many of the data structures we use for
    /// the pointer analysis have a "global" part that's shared between all functions and a "local"
    /// part that's specific to the function being analyzed; this struct contains only the local
    /// parts.  The different fields are set, used, and cleared at various points below.
    #[derive(Default)]
    struct FuncInfo<'tcx> {
        /// Local analysis context data, such as `LTy`s for all MIR locals.  Combine with the
        /// `GlobalAnalysisCtxt` to get a complete `AnalysisCtxt` for use within this function.
        acx_data: MaybeUnset<AnalysisCtxtData<'tcx>>,
        /// Dataflow constraints gathered from the body of this function.  These are used for
        /// propagating `READ`/`WRITE`/`OFFSET_ADD` and similar permissions.
        dataflow: MaybeUnset<DataflowConstraints>,
        /// Local equivalence-class information.  Combine with the `GlobalEquivSet` to get a
        /// complete `EquivSet`, which assigns an equivalence class to each `PointerId` that
        /// appears in the function.  Used for renumbering `PointerId`s.
        local_equiv: MaybeUnset<LocalEquivSet>,
        /// Local part of the permission/flag assignment.  Combine with the `GlobalAssignment` to
        /// get a complete `Assignment` for this function, which maps every `PointerId` in this
        /// function to a `PermissionSet` and `FlagSet`.
        lasn: MaybeUnset<LocalAssignment>,
    }

    // Assign global `PointerId`s for all pointers that appear in function signatures.
    for ldid in tcx.hir().body_owners() {
        let sig = tcx.fn_sig(ldid.to_def_id());
        let sig = tcx.erase_late_bound_regions(sig);

        let inputs = sig
            .inputs()
            .iter()
            .map(|&ty| gacx.assign_pointer_ids(ty))
            .collect::<Vec<_>>();
        let inputs = gacx.lcx.mk_slice(&inputs);
        let output = gacx.assign_pointer_ids(sig.output());

        let lsig = LFnSig { inputs, output };
        gacx.fn_sigs.insert(ldid.to_def_id(), lsig);
    }

    // Initial pass to assign local `PointerId`s and gather equivalence constraints, which state
    // that two pointer types must be converted to the same reference type.  Some additional data
    // computed during this the process is kept around for use in later passes.
    let mut global_equiv = GlobalEquivSet::new(gacx.num_pointers());
    for ldid in tcx.hir().body_owners() {
        let ldid_const = WithOptConstParam::unknown(ldid);
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();
        let lsig = gacx.fn_sigs.get(&ldid.to_def_id()).unwrap().clone();

        let mut acx = gacx.function_context(&mir);

        // Assign PointerIds to local types
        assert!(acx.local_tys.is_empty());
        acx.local_tys = IndexVec::with_capacity(mir.local_decls.len());
        for (local, decl) in mir.local_decls.iter_enumerated() {
            let lty = match mir.local_kind(local) {
                LocalKind::Var | LocalKind::Temp => acx.assign_pointer_ids(decl.ty),
                LocalKind::Arg => {
                    debug_assert!(local.as_usize() >= 1 && local.as_usize() <= mir.arg_count);
                    lsig.inputs[local.as_usize() - 1]
                }
                LocalKind::ReturnPointer => lsig.output,
            };
            let l = acx.local_tys.push(lty);
            assert_eq!(local, l);

            let ptr = acx.new_pointer();
            let l = acx.addr_of_local.push(ptr);
            assert_eq!(local, l);
        }

        // Compute local equivalence classes and dataflow constraints.
        let (dataflow, equiv_constraints) = dataflow::generate_constraints(&acx, &mir);
        let mut local_equiv = LocalEquivSet::new(acx.num_pointers());
        let mut equiv = global_equiv.and_mut(&mut local_equiv);
        for (a, b) in equiv_constraints {
            equiv.unify(a, b);
        }

        let mut info = FuncInfo::default();
        info.acx_data.set(acx.into_data());
        info.dataflow.set(dataflow);
        info.local_equiv.set(local_equiv);
        func_info.insert(ldid, info);
    }

    // Remap pointers based on equivalence classes, so all members of an equivalence class now use
    // the same `PointerId`.
    let (global_counter, global_equiv_map) = global_equiv.renumber();
    eprintln!("global_equiv_map = {:?}", global_equiv_map);
    gacx.remap_pointers(&global_equiv_map, global_counter);

    for ldid in tcx.hir().body_owners() {
        let info = func_info.get_mut(&ldid).unwrap();
        let (local_counter, local_equiv_map) = info.local_equiv.renumber(&global_equiv_map);
        eprintln!("local_equiv_map = {local_equiv_map:?}");
        info.acx_data.remap_pointers(
            gacx.lcx,
            global_equiv_map.and(&local_equiv_map),
            local_counter,
        );
        info.dataflow
            .remap_pointers(global_equiv_map.and(&local_equiv_map));
        info.local_equiv.clear();
    }

    // Compute permission and flag assignments.

    let mut gasn =
        GlobalAssignment::new(gacx.num_pointers(), PermissionSet::UNIQUE, FlagSet::empty());
    for info in func_info.values_mut() {
        let num_pointers = info.acx_data.num_pointers();
        let lasn = LocalAssignment::new(num_pointers, PermissionSet::UNIQUE, FlagSet::empty());
        info.lasn.set(lasn);
    }

    // Follow a postorder traversal, so that callers are visited after their callees.  This means
    // callee signatures will usually be up to date when we visit the call site.
    let order = body_owners_postorder(tcx);
    eprintln!("callgraph traversal order:");
    for &ldid in &order {
        eprintln!("  {ldid:?}");
    }
    let mut loop_count = 0;
    loop {
        // Loop until the global assignment reaches a fixpoint.  The inner loop also runs until a
        // fixpoint, but it only considers a single function at a time.  The inner loop for one
        // function can affect other functions by updating the `GlobalAssignment`, so we also need
        // the outer loop, which runs until the `GlobalAssignment` converges as well.
        loop_count += 1;
        let old_gasn = gasn.clone();
        for &ldid in &order {
            let info = func_info.get_mut(&ldid).unwrap();
            let ldid_const = WithOptConstParam::unknown(ldid);
            let name = tcx.item_name(ldid.to_def_id());
            let mir = tcx.mir_built(ldid_const);
            let mir = mir.borrow();

            let acx = gacx.function_context_with_data(&mir, info.acx_data.take());
            let mut asn = gasn.and(&mut info.lasn);

            // `dataflow.propagate` and `borrowck_mir` both run until the assignment converges on a
            // fixpoint, so there's no need to do multiple iterations here.
            info.dataflow.propagate(&mut asn.perms_mut());

            borrowck::borrowck_mir(
                &acx,
                &info.dataflow,
                &mut asn.perms_mut(),
                name.as_str(),
                &mir,
            );

            info.acx_data.set(acx.into_data());
        }

        if gasn == old_gasn {
            break;
        }
    }
    eprintln!("reached fixpoint in {} iterations", loop_count);

    // Print results for each function.
    for ldid in tcx.hir().body_owners() {
        let info = func_info.get_mut(&ldid).unwrap();
        let ldid_const = WithOptConstParam::unknown(ldid);
        let name = tcx.item_name(ldid.to_def_id());
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();
        let acx = gacx.function_context_with_data(&mir, info.acx_data.take());
        let mut asn = gasn.and(&mut info.lasn);
        info.dataflow.propagate_cell(&mut asn);

        // Print labeling and rewrites for the current function.

        eprintln!("final labeling for {:?}:", name);
        let lcx1 = crate::labeled_ty::LabeledTyCtxt::new(tcx);
        let lcx2 = crate::labeled_ty::LabeledTyCtxt::new(tcx);
        for (local, decl) in mir.local_decls.iter_enumerated() {
            let addr_of1 = asn.perms()[acx.addr_of_local[local]];
            let ty1 = lcx1.relabel(acx.local_tys[local], &mut |lty| {
                if lty.label == PointerId::NONE {
                    PermissionSet::empty()
                } else {
                    asn.perms()[lty.label]
                }
            });
            eprintln!(
                "{:?} ({}): addr_of = {:?}, type = {:?}",
                local,
                describe_local(tcx, decl),
                addr_of1,
                ty1,
            );

            let addr_of2 = asn.flags()[acx.addr_of_local[local]];
            let ty2 = lcx2.relabel(acx.local_tys[local], &mut |lty| {
                if lty.label == PointerId::NONE {
                    FlagSet::empty()
                } else {
                    asn.flags()[lty.label]
                }
            });
            eprintln!(
                "{:?} ({}): addr_of flags = {:?}, type flags = {:?}",
                local,
                describe_local(tcx, decl),
                addr_of2,
                ty2,
            );

            let addr_of3 = acx.addr_of_local[local];
            let ty3 = acx.local_tys[local];
            eprintln!(
                "{:?} ({}): addr_of = {:?}, type = {:?}",
                local,
                describe_local(tcx, decl),
                addr_of3,
                ty3,
            );
        }

        eprintln!("\ntype assignment for {:?}:", name);
        for (local, decl) in mir.local_decls.iter_enumerated() {
            // TODO: apply `Cell` if `addr_of_local` indicates it's needed
            let ty = type_desc::convert_type(&acx, acx.local_tys[local], &asn);
            eprintln!("{:?} ({}): {:?}", local, describe_local(tcx, decl), ty,);
        }

        eprintln!();
        let rewrites = expr_rewrite::gen_expr_rewrites(&acx, &asn, &mir);
        for rw in &rewrites {
            eprintln!(
                "at {:?} ({}, {:?}):",
                rw.loc.stmt,
                describe_span(tcx, rw.loc.span),
                rw.loc.sub,
            );
            for kind in &rw.kinds {
                eprintln!("  {:?}", kind);
            }
        }
    }
}

trait AssignPointerIds<'tcx> {
    fn lcx(&self) -> LTyCtxt<'tcx>;

    fn new_pointer(&mut self) -> PointerId;

    fn assign_pointer_ids(&mut self, ty: Ty<'tcx>) -> LTy<'tcx> {
        self.lcx().label(ty, &mut |ty| match ty.kind() {
            TyKind::Ref(_, _, _) | TyKind::RawPtr(_) => self.new_pointer(),
            _ => PointerId::NONE,
        })
    }
}

impl<'tcx> AssignPointerIds<'tcx> for GlobalAnalysisCtxt<'tcx> {
    fn lcx(&self) -> LTyCtxt<'tcx> {
        self.lcx
    }

    fn new_pointer(&mut self) -> PointerId {
        self.new_pointer()
    }
}

impl<'tcx> AssignPointerIds<'tcx> for AnalysisCtxt<'_, 'tcx> {
    fn lcx(&self) -> LTyCtxt<'tcx> {
        self.lcx()
    }

    fn new_pointer(&mut self) -> PointerId {
        self.new_pointer()
    }
}

fn describe_local(tcx: TyCtxt, decl: &LocalDecl) -> String {
    let mut span = decl.source_info.span;
    if let Some(ref info) = decl.local_info {
        if let LocalInfo::User(ref binding_form) = **info {
            let binding_form = binding_form.as_ref().assert_crate_local();
            if let BindingForm::Var(ref v) = *binding_form {
                span = v.pat_span;
            }
        }
    }
    describe_span(tcx, span)
}

fn describe_span(tcx: TyCtxt, span: Span) -> String {
    let s = tcx.sess.source_map().span_to_snippet(span).unwrap();
    let s = {
        let mut s2 = String::new();
        for word in s.split_ascii_whitespace() {
            if !s2.is_empty() {
                s2.push(' ');
            }
            s2.push_str(word);
        }
        s2
    };

    let (src1, src2, src3) = if s.len() > 20 {
        (&s[..15], " ... ", &s[s.len() - 5..])
    } else {
        (&s[..], "", "")
    };
    let line = tcx.sess.source_map().lookup_char_pos(span.lo()).line;
    format!("{}: {}{}{}", line, src1, src2, src3)
}

/// Return all `body_owners`, ordered according to a postorder traversal of the graph of references
/// between bodies.
fn body_owners_postorder(tcx: TyCtxt) -> Vec<LocalDefId> {
    let mut seen = HashSet::new();
    let mut order = Vec::new();
    enum Visit {
        Pre(LocalDefId),
        Post(LocalDefId),
    }
    let mut stack = Vec::new();

    for root_ldid in tcx.hir().body_owners() {
        if seen.contains(&root_ldid) {
            continue;
        }
        stack.push(Visit::Pre(root_ldid));
        while let Some(visit) = stack.pop() {
            match visit {
                Visit::Pre(ldid) => {
                    if seen.insert(ldid) {
                        stack.push(Visit::Post(ldid));
                        for_each_callee(tcx, ldid, |callee_ldid| {
                            stack.push(Visit::Pre(callee_ldid));
                        });
                    }
                }
                Visit::Post(ldid) => {
                    order.push(ldid);
                }
            }
        }
    }

    order
}

fn for_each_callee(tcx: TyCtxt, ldid: LocalDefId, f: impl FnMut(LocalDefId)) {
    let ldid_const = WithOptConstParam::unknown(ldid);
    let mir = tcx.mir_built(ldid_const);
    let mir = mir.borrow();
    let mir: &Body = &mir;

    struct CalleeVisitor<'a, 'tcx, F> {
        tcx: TyCtxt<'tcx>,
        mir: &'a Body<'tcx>,
        f: F,
    }

    impl<'tcx, F: FnMut(LocalDefId)> Visitor<'tcx> for CalleeVisitor<'_, 'tcx, F> {
        fn visit_operand(&mut self, operand: &Operand<'tcx>, _location: Location) {
            let ty = operand.ty(self.mir, self.tcx);
            if let Some(Callee::Other { def_id, .. }) = util::ty_callee(self.tcx, ty) {
                if let Some(ldid) = def_id.as_local() {
                    (self.f)(ldid);
                }
            }
        }
    }

    CalleeVisitor { tcx, mir, f }.visit_body(mir);
}

struct AnalysisCallbacks;

impl rustc_driver::Callbacks for AnalysisCallbacks {
    fn after_expansion<'tcx>(
        &mut self,
        _compiler: &rustc_interface::interface::Compiler,
        queries: &'tcx rustc_interface::Queries<'tcx>,
    ) -> rustc_driver::Compilation {
        queries.global_ctxt().unwrap().peek_mut().enter(|tcx| {
            run(tcx);
        });
        rustc_driver::Compilation::Continue
    }
}

fn main() -> rustc_interface::interface::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    rustc_driver::RunCompiler::new(&args, &mut AnalysisCallbacks).run()
}
