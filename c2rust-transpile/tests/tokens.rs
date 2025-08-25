use syn::__private::ToTokens;

#[test]
fn test_tokenize() {
    use c2rust_ast_builder::mk;
    let tys = vec![mk().path_ty(vec!["t"])];
    let args = mk().angle_bracketed_args(tys);
    let pathsegment = mk().path_segment_with_args("x", args);
    assert_eq!(pathsegment.to_token_stream().to_string(), "x :: < t >");

    let path = mk().path(vec![pathsegment]);
    assert_eq!(path.to_token_stream().to_string(), "x :: < t > ::");
    // bug: path should not end with Colon2
}
