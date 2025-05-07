use crate::anonymous_decls::rust_k;

pub fn test_anonymous_decl() {
    unsafe {
        assert_eq!(rust_k.j.l, 0);
    }
}
