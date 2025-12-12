use crate::error::Error;
use std::collections::HashSet;
use std::fs;
use std::iter;
use std::path::{Path, PathBuf};
use syn;
use syn::ext::IdentExt;

#[derive(Clone, Default)]
pub struct FileCollector {
    /// File path, module path, and AST for each file visited so far.
    pub files: Vec<(PathBuf, Vec<String>, syn::File)>,
    seen: HashSet<PathBuf>,
}

impl FileCollector {
    pub fn parse(
        &mut self,
        file_path: impl AsRef<Path>,
        mod_path: Vec<String>,
        is_root: bool,
    ) -> Result<(), Error> {
        let file_path = file_path.as_ref();
        if self.seen.contains(file_path) {
            return Ok(());
        }
        let src = fs::read_to_string(file_path)
            .map_err(|e| Error::from(e).at(format_args!("reading {file_path:?}")))?;
        let ast: syn::File = syn::parse_file(&src)
            .map_err(|e| Error::from(e).at(format_args!("parsing {file_path:?}")))?;
        // Set `seen` immediately, but don't add to `files` (and give up ownership) until we're
        // done walking `ast`.
        self.seen.insert(file_path.to_owned());
        let is_mod_rs = is_root || file_path.file_name().is_some_and(|n| n == "mod.rs");
        let base_path_storage;
        let base_path = if is_mod_rs {
            file_path
                .parent()
                .ok_or_else(|| format!("mod.rs path {file_path:?} has no parent"))?
        } else {
            base_path_storage = file_path.with_extension("");
            &base_path_storage
        };
        self.walk_items(&ast.items, base_path, mod_path.clone(), &[])?;
        self.files.push((file_path.to_owned(), mod_path, ast));
        Ok(())
    }

    fn walk_items(
        &mut self,
        items: &[syn::Item],
        base_path: &Path,
        mut mod_path: Vec<String>,
        parent_module: &[&str],
    ) -> Result<(), Error> {
        for item in items {
            let im = match *item {
                syn::Item::Mod(ref im) => im,
                _ => continue,
            };
            mod_path.push(im.ident.unraw().to_string());
            if let Some((_, ref inline_items)) = im.content {
                let name =
                    path_attr_value(&im.attrs)?.unwrap_or_else(|| im.ident.unraw().to_string());
                let module = parent_module
                    .iter()
                    .copied()
                    .chain(iter::once(&name as &_))
                    .collect::<Vec<_>>();
                self.walk_items(inline_items, base_path, mod_path.clone(), &module)?;
            } else {
                let mut path = base_path.to_owned();
                for &m in parent_module {
                    path.push(m);
                }
                if let Some(attr_path) = path_attr_value(&im.attrs)? {
                    path.push(attr_path);
                    self.parse(path, mod_path.clone(), false)?;
                } else {
                    let name = im.ident.unraw().to_string();
                    // Try `foo/mod.rs` first; if it doesn't exist, try `foo.rs` instead.
                    path.push(name);
                    path.push("mod.rs");
                    if !fs::exists(&path)? {
                        path.pop();
                        path.set_extension("rs");
                    }
                    self.parse(path, mod_path.clone(), false)?;
                }
            }
            mod_path.pop();
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
