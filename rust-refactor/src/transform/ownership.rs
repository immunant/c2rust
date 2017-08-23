use std::collections::HashSet;

use rustc::hir;
use rustc_data_structures::indexed_vec::IndexVec;
use syntax::ast::*;
use syntax::codemap::DUMMY_SP;
use syntax::fold::{self, Folder};
use syntax::parse::token::{self, Token, DelimToken};
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::tokenstream::{TokenTree, TokenStream, Delimited};
use syntax::util::small_vector::SmallVector;
use syntax::util::move_map::MoveMap;

use analysis::ownership::{self, ConcretePerm, Var, PTy};
use analysis::ownership::constraint::{ConstraintSet, Perm};
use command::{CommandState, Registry, DriverCommand};
use driver::{self, Phase};
use fold::Fold;
use make_ast::mk;
use util::IntoSymbol;

pub fn register_commands(reg: &mut Registry) {
    reg.register("ownership_annotate", |args| {
        let label = args.get(0).map_or("target", |x| x).into_symbol();

        Box::new(DriverCommand::new(Phase::Phase3, move |st, cx| {
            do_annotate(st, cx, label);
        }))
    });
}

fn do_annotate(st: &CommandState,
               cx: &driver::Ctxt,
               label: Symbol) {
    let analysis = ownership::analyze(&st, &cx);

    struct AnnotateFolder<'a, 'hir: 'a, 'tcx> {
        label: Symbol,
        ana: ownership::AnalysisResult<'tcx>,
        hir_map: &'a hir::map::Map<'hir>,
        st: &'a CommandState,
    }

    impl<'a, 'hir, 'tcx> AnnotateFolder<'a, 'hir, 'tcx> {
        fn static_attr_for(&self, id: NodeId) -> Option<Attribute> {
            self.hir_map.opt_local_def_id(id)
                .and_then(|def_id| self.ana.statics.get(&def_id))
                .and_then(|&ty| build_static_attr(ty))
        }

        fn constraints_attr_for(&self, id: NodeId) -> Option<Attribute> {
            self.hir_map.opt_local_def_id(id)
                .and_then(|def_id| self.ana.fns.get(&def_id))
                .map(|fr| build_constraints_attr(&fr.cset))
        }

        fn push_mono_attrs_for(&self, id: NodeId, dest: &mut Vec<Attribute>) {
            if let Some(fr) = self.hir_map.opt_local_def_id(id)
                    .and_then(|def_id| self.ana.fns.get(&def_id)) {
                if fr.num_sig_vars == 0 {
                    return;
                }

                for mr in &fr.monos {
                    dest.push(build_mono_attr(&mr.suffix, &mr.assign));
                }
            }
        }
    }

    impl<'a, 'hir, 'tcx> Folder for AnnotateFolder<'a, 'hir, 'tcx> {
        fn fold_item(&mut self, i: P<Item>) -> SmallVector<P<Item>> {
            if !self.st.marked(i.id, self.label) {
                return fold::noop_fold_item(i, self);
            }

            fold::noop_fold_item(i.map(|mut i| {
                match i.node {
                    ItemKind::Static(..) | ItemKind::Const(..) => {
                        if let Some(attr) = self.static_attr_for(i.id) {
                            i.attrs.push(attr);
                        }
                    },

                    ItemKind::Fn(..) => {
                        if let Some(attr) = self.constraints_attr_for(i.id) {
                            i.attrs.push(attr);
                        }
                        self.push_mono_attrs_for(i.id, &mut i.attrs);
                    },

                    _ => {},
                }

                i
            }), self)
        }

        fn fold_impl_item(&mut self, i: ImplItem) -> SmallVector<ImplItem> {
            if !self.st.marked(i.id, self.label) {
                return fold::noop_fold_impl_item(i, self);
            }

            fold::noop_fold_impl_item(i, self)
        }

        fn fold_struct_field(&mut self, mut sf: StructField) -> StructField {
            if !self.st.marked(sf.id, self.label) {
                return fold::noop_fold_struct_field(sf, self);
            }

            if let Some(attr) = self.static_attr_for(sf.id) {
                sf.attrs.push(attr);
            }

            fold::noop_fold_struct_field(sf, self)
        }
    }

    st.map_krate(|krate| {
        krate.fold(&mut AnnotateFolder {
            label: label,
            ana: analysis,
            hir_map: cx.hir_map(),
            st: st,
        })
    });
}

fn build_static_attr(ty: PTy) -> Option<Attribute> {
    let mut args = Vec::new();
    ty.for_each_label(&mut |p| {
        if let Some(p) = *p {
            args.push(perm_token(p));
        }
    });
    let tokens = parens(args).into();
    Some(make_attr("ownership_static", tokens))
}

fn build_constraints_attr(cset: &ConstraintSet) -> Attribute {
    let mut args = Vec::new();

    fn push_perm_tokens(p: Perm, dest: &mut Vec<TokenTree>) {
        match p {
            Perm::Concrete(p) => dest.push(perm_token(p)),
            Perm::SigVar(v) => dest.push(ident_token(&format!("_{}", v.0))),
            Perm::Min(ps) => {
                let mut ts = Vec::new();
                for (i, &p) in ps.iter().enumerate() {
                    if i > 0 {
                        ts.push(token(Token::Comma));
                    }
                    push_perm_tokens(p, &mut ts);
                }
                dest.push(ident_token("min"));
                dest.push(parens(ts));
            },
            _ => panic!("unexpected var kind in fn constraints"),
        }
    }

    for (i, &(a, b)) in cset.iter().enumerate() {
        if i > 0 {
            args.push(token(Token::Comma));
        }
        args.push(ident_token("le"));

        let mut le_args = Vec::new();
        push_perm_tokens(a, &mut le_args);
        le_args.push(token(Token::Comma));
        push_perm_tokens(b, &mut le_args);

        args.push(parens(le_args));
    }

    let tokens = parens(args).into();
    make_attr("ownership_constraints", tokens)
}

fn build_mono_attr(suffix: &str, assign: &IndexVec<Var, ConcretePerm>) -> Attribute {
    let mut args = Vec::new();
    args.push(str_token(suffix));

    for &p in assign.iter() {
        args.push(token(Token::Comma));
        args.push(perm_token(p));
    }

    let tokens = parens(args).into();
    make_attr("ownership_mono", tokens)
}

fn perm_token(p: ConcretePerm) -> TokenTree {
    let name = match p {
        ConcretePerm::Read => "READ",
        ConcretePerm::Write => "WRITE",
        ConcretePerm::Move => "MOVE",
    };
    TokenTree::Token(DUMMY_SP, Token::Ident(mk().ident(name)))
}

fn ident_token(name: &str) -> TokenTree {
    token(Token::Ident(mk().ident(name)))
}

fn str_token(s: &str) -> TokenTree {
    token(Token::Literal(token::Lit::Str_(s.into_symbol()), None))
}

fn token(t: Token) -> TokenTree {
    TokenTree::Token(DUMMY_SP, t)
}

fn parens(ts: Vec<TokenTree>) -> TokenTree {
    TokenTree::Delimited(DUMMY_SP, Delimited {
        delim: DelimToken::Paren,
        tts: ts.into_iter().collect::<TokenStream>().into(),
    })
}

fn make_attr(name: &str, tokens: TokenStream) -> Attribute {
    Attribute {
        id: AttrId(0),
        style: AttrStyle::Outer,
        path: mk().path(vec![name]),
        tokens: tokens,
        is_sugared_doc: false,
        span: DUMMY_SP,
    }
}
