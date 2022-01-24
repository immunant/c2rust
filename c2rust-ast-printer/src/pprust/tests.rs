use super::*;

use crate::syn;

/*fn fun_to_string(
    decl: &ast::FnDecl, header: ast::FnHeader, name: ast::Ident, generics: &ast::Generics
) -> String {
    to_string(|s| {
        s.head("");
        s.print_fn(decl, header, Some(name),
                   generics, &source_map::dummy_spanned(ast::VisibilityKind::Inherited));
        s.end(); // Close the head box.
        s.end(); // Close the outer box.
    })
}*/

fn variant_to_string(var: &syn::Variant) -> String {
    to_string(|s| s.print_variant(var))
}

#[test]
fn test_fun_to_string() {
    //with_default_globals(|| {
        let abba_ident = syn::Ident::new("abba", proc_macro2::Span::call_site());

        let decl = FnDecl {
            inputs: Vec::new(),
            output: syn::ReturnType::Default,
        };
        let generics = syn::Generics::default();
        assert_eq!(
            fun_to_string(
                &decl,
                ast::FnHeader::default(),
                abba_ident,
                &generics
            ),
            "fn abba()"
        );
    //})
}

#[test]
fn test_variant_to_string() {
    //with_default_globals(|| {
        let ident = syn::Ident::new("principal_skinner", proc_macro2::Span::call_site());

        let var = syn::Variant {
            attrs: vec![],
            discriminant: None,
            fields: todo!(),
            ident,
        };

        let varstr = variant_to_string(&var);
        assert_eq!(varstr, "principal_skinner");
    //})
}
