use proc_macro2::{TokenStream, TokenTree};

#[cfg(test)]
mod tests;


#[derive(PartialEq, Eq, Debug, PartialOrd, Ord, Copy, Clone)]
pub struct BytePos(pub u32);

pub mod comments {
    #[derive(Clone)]
    pub struct Comment {
        pub lines: Vec<String>,
        pub pos: super::BytePos,
    }
}

pub enum MacHeader<'a> {
    Path(&'a syn::Path),
    Keyword(&'static str),
}

pub struct Comments {
    //cm: &'a SourceMap,
    pub comments: Vec<comments::Comment>,
    current: usize,
}

impl Comments {
    pub fn new(
        //cm: &'a SourceMap,
        comments: Vec<comments::Comment>,
    ) -> Comments {
        Comments {
            //cm,
            comments,
            current: 0,
        }
    }

    // pub fn parse(
    //     cm: &'a SourceMap,
    //     sess: &ParseSess,
    //     filename: FileName,
    //     input: String,
    // ) -> Comments<'a> {
    //     let comments = comments::gather_comments(sess, filename, input);
    //     Comments {
    //         cm,
    //         comments,
    //         current: 0,
    //     }
    // }

    pub fn next(&self) -> Option<comments::Comment> {
        self.comments.get(self.current).cloned()
    }

    pub fn trailing_comment(
        &mut self,
        span: proc_macro2::Span,
        next_pos: Option<usize>,
    ) -> Option<String> {
        /*if let Some(cmnt) = self.next() {
            if cmnt.style != comments::Trailing { return None; }
            let span_line = self.cm.lookup_char_pos(span.hi());
            let comment_line = self.cm.lookup_char_pos(cmnt.pos);
            let next = next_pos.unwrap_or_else(|| cmnt.pos + BytePos(1));
            if span.hi() < cmnt.pos && cmnt.pos < next && span_line.line == comment_line.line {
                return Some(cmnt);
            }
        }*/

        None
    }
}

impl Extend<comments::Comment> for Comments {
    fn extend<I>(&mut self, iter: I)
        where I: IntoIterator<Item = comments::Comment>
    {
        self.comments.extend(iter);
    }
}
    

pub struct State {
    pub s: prettyplease::Printer,
    comments: Option<Comments>,
    is_expanded: bool
}

pub fn to_string<F>(f: F) -> String where
    F: FnOnce(&mut State),
{
    let mut printer = State {
        s: prettyplease::Printer::new(),
        comments: None,
        is_expanded: false
    };
    f(&mut printer);
    printer.s.eof()
}

pub fn to_string_with_comments<F>(comments: Comments, f: F) -> String where
    F: FnOnce(&mut State)
{
    let mut printer = State {
        s: prettyplease::Printer::new(),
        comments: Some(comments),
        is_expanded: false
    };
    f(&mut printer);
    printer.s.eof()
}

pub fn literal_to_string(lit: syn::Lit) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.lit(&lit);
    pr.eof()
}

/// Print an ident from AST, `$crate` is converted into its respective crate name.
pub fn ast_ident_to_string(ident: syn::Ident, is_raw: bool) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.ident(&ident);
    pr.eof()
}

pub fn ty_to_string(ty: &syn::Type) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.ty(ty);
    pr.eof()
}

pub fn pat_to_string(pat: &syn::Pat) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.pat(pat);
    pr.eof()
}

pub fn expr_to_string(e: &syn::Expr) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.expr(e);
    pr.eof()
}

pub fn tt_to_string(tt: TokenTree) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.single_token(tt.into(), |printer, stream| printer.macro_rules_tokens(stream, true));
    pr.eof()
}

pub fn tts_to_string(tokens: TokenStream) -> String {

    //tokens.to_string()
    let mut pr = prettyplease::Printer::new();
    pr.macro_rules_tokens(tokens, true);
    //pr.tts(tokens);
    pr.eof()
}

pub fn stmt_to_string(stmt: &syn::Stmt) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.stmt(stmt);
    pr.eof()
}

pub fn item_to_string(i: &syn::Item) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.item(i);
    pr.eof()
}

pub fn impl_item_to_string(i: &syn::ImplItem) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.impl_item(i);
    pr.eof()
}

pub fn trait_item_to_string(i: &syn::TraitItem) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.trait_item(i);
    pr.eof()
}

pub fn generic_params_to_string(generic_params: &[syn::GenericParam]) -> String {
    let mut pr = prettyplease::Printer::new();
    for g in generic_params {
        pr.generic_param(g);
    }
    pr.eof()
}

pub fn path_to_string(p: &syn::Path) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.path(p);
    pr.eof()
}

pub fn path_segment_to_string(p: &syn::PathSegment) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.path_segment(p);
    pr.eof()
}

pub fn vis_to_string(v: &syn::Visibility) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.visibility(v);
    pr.eof()
}

pub fn block_to_string(blk: &syn::Block) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.expr_block(&syn::ExprBlock {attrs: vec![], label: None, block: blk.clone()});
    pr.eof()
}

pub fn attribute_to_string(attr: &syn::Attribute) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.attr(attr);
    pr.eof()
}

pub fn param_to_string(arg: &syn::FnArg) -> String {
    let mut pr = prettyplease::Printer::new();
    let _ = pr.maybe_variadic(arg);
    pr.eof()
}

pub fn foreign_item_to_string(arg: &syn::ForeignItem) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.foreign_item(arg);
    pr.eof()
}

pub fn visibility_qualified(vis: &syn::Visibility, s: &str) -> String {
    let mut pr = prettyplease::Printer::new();
    pr.visibility(vis);
    let mut out = pr.eof();
    out += s;
    out
}

impl std::ops::Deref for State {
    type Target = prettyplease::Printer;
    fn deref(&self) -> &Self::Target {
        &self.s
    }
}

impl std::ops::DerefMut for State {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.s
    }
}
