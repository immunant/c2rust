use std::collections::{HashMap, HashSet};
use rustc::hir::def_id::DefId;
use rustc::session::Session;
use rustc::ty::TypeVariants;
use syntax::abi::Abi;
use syntax::ast::*;
use syntax::codemap::{Spanned, DUMMY_SP};
use syntax::ext::base::{ExtCtxt, Resolver, DummyResolver};
use syntax::ext::expand::ExpansionConfig;
use syntax::ext::quote::rt::ToTokens;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::parse::token::Token;
use syntax::tokenstream::TokenTree;
use syntax::util::small_vector::SmallVector;

use api::*;
use bindings::Bindings;
use command::{CommandState, Registry};
use driver::{self, Phase};
use fold::Fold;
use transform::Transform;
use util::IntoSymbol;


/// Replace uses of a target function with invocations of a macro.
pub struct FuncToMacro {
    macro_name: Symbol,
}

fn build_ext_ctxt<'a>(sess: &'a Session, resolver: &'a mut Resolver) -> ExtCtxt<'a> {
    let cfg = ExpansionConfig::default("<dummy>".to_owned());
    ExtCtxt::new(&sess.parse_sess, cfg, resolver)
}

impl Transform for FuncToMacro {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let funcs = st.marks().iter()
            .filter_map(|&(id, sym)|
                        if sym.as_str() == "target" { Some(cx.node_def_id(id)) }
                        else { None })
            .collect::<HashSet<_>>();

        fold_nodes(krate, |e: P<Expr>| {
            let callee = match_or!([cx.opt_callee(&e)] Some(x) => x; return e);
            if !funcs.contains(&callee) {
                return e;
            }

            let args = match e.unwrap().node {
                ExprKind::Call(_, args) => args,
                ExprKind::MethodCall(_, args) => args,
                _ => panic!("expected Call or MethodCall"),
            };

            let mut resolver = DummyResolver;
            let ext_ctxt = build_ext_ctxt(cx.session(), &mut resolver);
            let mut tts = Vec::new();
            for (i, arg) in args.into_iter().enumerate() {
                if i > 0 {
                    tts.push(TokenTree::Token(DUMMY_SP, Token::Comma));
                }
                tts.append(&mut arg.to_tokens(&ext_ctxt));
            }

            mk().mac_expr(mk().mac(vec![self.macro_name], tts))
        })
    }


    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}



pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("func_to_macro", |args| mk(FuncToMacro {
        macro_name: (&args[0]).into_symbol(),
    }));
}
