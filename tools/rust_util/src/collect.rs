use std::collections::HashSet;
use std::fs;
use std::iter;
use std::path::{Path, PathBuf};
use syn;
use crate::error::Error;


#[derive(Clone, Default)]
pub struct FileCollector {
    pub files: Vec<(PathBuf, syn::File)>,
    seen: HashSet<PathBuf>,
}

impl FileCollector {
    pub fn parse(
        &mut self,
        path: impl AsRef<Path>,
        is_root: bool,
    ) -> Result<(), Error> {
        let path = path.as_ref();
        if self.seen.contains(path) {
            return Ok(());
        }
        let src = fs::read_to_string(path)
            .map_err(|e| Error::from(e).at(format_args!("reading {path:?}")))?;
        let ast: syn::File = syn::parse_file(&src)
            .map_err(|e| Error::from(e).at(format_args!("parsing {path:?}")))?;
        // Set `seen` immediately, but don't add to `files` (and give up ownership) until we're
        // done walking `ast`.
        self.seen.insert(path.to_owned());
        let is_mod_rs = is_root || path.file_name().is_some_and(|n| n == "mod.rs");
        let base_path_storage;
        let base_path = if is_mod_rs {
            path.parent().ok_or_else(|| format!("mod.rs path {path:?} has no parent"))?
        } else {
            base_path_storage = path.with_extension("");
            &base_path_storage
        };
        self.walk_items(&ast.items, base_path, &[])?;
        self.files.push((path.to_owned(), ast));
        Ok(())
    }

    fn walk_items(
        &mut self,
        items: &[syn::Item],
        base_path: &Path,
        parent_module: &[&str],
    ) -> Result<(), Error> {
        for item in items {
            let im = match *item {
                syn::Item::Mod(ref im) => im,
                _ => continue,
            };
            if let Some((_, ref inline_items)) = im.content {
                let name = path_attr_value(&im.attrs)?
                    .unwrap_or_else(|| im.ident.to_string());
                let module = parent_module.iter().copied().chain(iter::once(&name as &_))
                    .collect::<Vec<_>>();
                self.walk_items(inline_items, base_path, &module)?;
            } else {
                let mut path = base_path.to_owned();
                for &m in parent_module {
                    path.push(m);
                }
                if let Some(attr_path) = path_attr_value(&im.attrs)? {
                    path.push(attr_path);
                    self.parse(path, false)?;
                } else {
                    let name = im.ident.to_string();
                    // Try `foo/mod.rs` first; if it doesn't exist, try `foo.rs` instead.
                    path.push(name);
                    path.push("mod.rs");
                    if !fs::exists(&path)? {
                        path.pop();
                        path.set_extension("rs");
                    }
                    self.parse(path, false)?;
                }
            }
        }
        Ok(())
    }
}

fn path_attr_value(attrs: &[syn::Attribute]) -> Result<Option<String>, Error> {
    for attr in attrs {
        if !attr.meta.path().is_ident("path") {
            continue;
        }
        let mnv = match attr.meta {
            syn::Meta::NameValue(ref x) => x,
            _ => return Err("expected `path` attribute to have a value".into()),
        };
        let el = match mnv.value {
            syn::Expr::Lit(ref x) => x,
            _ => return Err("expected `path` attribute value to be a literal".into()),
        };
        let ls = match el.lit {
            syn::Lit::Str(ref x) => x,
            _ => return Err("expected `path` attribute value to be a string literal".into()),
        };
        return Ok(Some(ls.value()));
    }
    Ok(None)
}
