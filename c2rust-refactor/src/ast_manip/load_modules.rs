use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_ast::ptr::P;
use rustc_ast::*;
use rustc_parse::new_parser_from_file;
use rustc_session::parse::ParseSess;
use rustc_span::source_map::SourceMap;
use rustc_span::FileName;
use smallvec::SmallVec;
use std::path::PathBuf;

struct LoadModules<'a> {
    parse_sess: &'a ParseSess,
    source_map: &'a SourceMap,
    dir_path: PathBuf,
}

impl<'a> MutVisitor for LoadModules<'a> {
    fn flat_map_item(&mut self, mut i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        let Item {
            attrs,
            span,
            ident,
            kind: ItemKind::Mod(_, mod_kind),
            ..
        } = &mut *i else {
            return mut_visit::noop_flat_map_item(i, self);
        };

        match mod_kind {
            ModKind::Loaded(_items, Inline::Yes, _spans) => {
                // TODO: handle #[path="..."]
            }

            ModKind::Loaded(_items, Inline::No, _spans) => {
                // We shouldn't be seeing any modules at this point that
                // were loaded from any file other than the crate root
                // (e.g. lib.rs)
                panic!("unexpected loaded outline module: {i:?}");
            }

            ModKind::Unloaded => {
                // Look for dir_path/foo.rs, then try dir_path/foo/mod.rs
                let mut mod_file_path = self.dir_path.join(ident.as_str()).with_extension("rs");
                if !self.source_map.file_exists(&mod_file_path) {
                    mod_file_path = self.dir_path.join(ident.as_str()).join("mod.rs");
                }
                if !self.source_map.file_exists(&mod_file_path) {
                    panic!("unable to load module file {mod_file_path:?}");
                }

                let mut parser =
                    new_parser_from_file(&self.parse_sess, &mod_file_path, Some(*span));
                let (mut inner_attrs, items, inner_span) = parser
                    .parse_mod(&token::Eof)
                    .expect("failed to parse {mod_file_path:?}");

                attrs.append(&mut inner_attrs);
                *mod_kind = ModKind::Loaded(items, Inline::No, inner_span);
            }
        }

        self.dir_path.push(ident.as_str());
        let res = mut_visit::noop_flat_map_item(i, self);
        self.dir_path.pop();

        res
    }
}

pub fn load_modules(krate: &mut Crate, parse_sess: &ParseSess, source_map: &SourceMap) {
    let file_path = match source_map.span_to_filename(krate.spans.inner_span) {
        FileName::Real(name) => name
            .into_local_path()
            .expect("attempting to resolve a file path in an external file"),
        other => panic!("crate is not from a real file: {other:?}"),
    };
    let dir_path = file_path.parent().unwrap_or(&file_path).to_owned();

    let mut lm = LoadModules {
        parse_sess,
        source_map,
        dir_path,
    };

    lm.visit_crate(krate);
}
