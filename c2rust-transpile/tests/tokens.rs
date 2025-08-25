use c2rust_ast_builder::mk;
use syn::__private::ToTokens;

#[test]
fn test_tokenize() {
    let tys = vec![mk().path_ty(vec!["t"])];
    let args = mk().angle_bracketed_args(tys);
    let path_segment = mk().path_segment_with_args("x", args);
    assert_eq!(path_segment.to_token_stream().to_string(), "x :: < t >");

    let path = mk().path(vec![path_segment]);
    assert_eq!(path.to_token_stream().to_string(), "x :: < t >");
}
