use super::*;

/// These tests mostly verify that we strip the right context from the
/// pretty-printed files that we have to insert non-toplevel syntactic elements
/// into in order to run them through prettyplease.

#[test]
fn test_expr_to_string() {
    assert_eq!(expr_to_string(&ret_expr()), "return");
}

#[test]
fn test_pat_to_string() {
    let wild_pat = syn::Pat::Wild(syn::PatWild {
        attrs: vec![],
        underscore_token: Default::default(),
    });
    assert_eq!(pat_to_string(&wild_pat), "_");
}

#[test]
fn test_path_to_string() {
    let name = "friendly_ident_name";
    let ident = syn::Ident::new(name, proc_macro2::Span::call_site());
    let path = syn::Path::from(ident);
    assert_eq!(path_to_string(&path), name);
}

#[test]
fn test_stmt_to_string() {
    let stmt = syn::Stmt::Expr(ret_expr(), Some(syn::token::Semi::default()));
    assert_eq!(stmt_to_string(&stmt), "return;");
}
