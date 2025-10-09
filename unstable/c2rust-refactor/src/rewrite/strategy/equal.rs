use crate::rewrite::RewriteCtxtRef;

/// World's simplest rewrite strategy: just hope the two ASTs are already equal!  Returns success
/// (and does nothing) if they are; returns failure (and still does nothing) if they aren't.
pub fn rewrite<T: PartialEq<T>>(old: &T, new: &T, _rcx: RewriteCtxtRef) -> bool {
    old == new
}
