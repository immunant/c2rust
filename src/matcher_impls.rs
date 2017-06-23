use syntax::ast::*;
use syntax::codemap::Spanned;
use syntax::ptr::P;

use matcher::{self, TryMatch, MatchCtxt};


impl TryMatch for Attribute {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result {
        Err(matcher::Error::NotImplemented)
    }
}

impl TryMatch for Ty {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result {
        Err(matcher::Error::NotImplemented)
    }
}

impl TryMatch for Ident {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result {
        if self.name.as_str().starts_with("__") {
            mcx.try_capture_ident(self.name, target)?;
            return Ok(());
        }

        mcx.try_match(&self.name, &target.name)?;
        Ok(())
    }
}

impl TryMatch for Name {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result {
        if self == target {
            Ok(())
        } else {
            Err(matcher::Error::SymbolMismatch)
        }
    }
}

impl TryMatch for Expr {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result {
        if let Ok(()) = mcx.try_capture_expr(self, target) {
            return Ok(());
        }

        mcx.try_match(&self.node, &target.node)?;
        mcx.try_match(&self.attrs, &target.attrs)?;
        Ok(())
    }
}

impl TryMatch for ExprKind {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result {
        match (self, target) {
            (&ExprKind::Call(ref func1, ref args1),
             &ExprKind::Call(ref func2, ref args2)) => {
                mcx.try_match(func1, func2)?;
                mcx.try_match(args1, args2)?;
            },

            (&ExprKind::MethodCall(ref id1, ref tys1, ref args1),
             &ExprKind::MethodCall(ref id2, ref tys2, ref args2)) => {
                mcx.try_match(id1, id2)?;
                mcx.try_match(tys1, tys2)?;
                mcx.try_match(args1, args2)?;
            },

            (_, _) => return Err(matcher::Error::VariantMismatch),
        }
        Ok(())
    }
}


impl<T: TryMatch> TryMatch for [T] {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result {
        if self.len() != target.len() {
            return Err(matcher::Error::LengthMismatch);
        }
        for i in 0 .. self.len() {
            mcx.try_match(&self[i], &target[i])?;
        }
        Ok(())
    }
}

impl<T: TryMatch> TryMatch for Vec<T> {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result {
        <[T] as TryMatch>::try_match(self, target, mcx)
    }
}

impl<T: TryMatch> TryMatch for ThinVec<T> {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result {
        <[T] as TryMatch>::try_match(self, target, mcx)
    }
}

impl<T: TryMatch> TryMatch for P<T> {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result {
        mcx.try_match(&**self, &**target)
    }
}

impl<T: TryMatch> TryMatch for Spanned<T> {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> matcher::Result {
        mcx.try_match(&self.node, &target.node)
    }
}

