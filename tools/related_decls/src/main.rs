use clap::Parser;
use itertools::Itertools;
use ra_ap_hir::{
    Adt, Function, HasCrate, HasSource, InFile, Module, ModuleDef, ModuleDefId, Name, Semantics,
    db::DefDatabase,
};
use ra_ap_hir_def::{hir::generics::TypeOrConstParamData, signatures::FunctionSignature};
use ra_ap_ide_db::search::FileReferenceNode;
use ra_ap_ide_db::{RootDatabase, defs::Definition};
use ra_ap_load_cargo::{self, LoadCargoConfig, ProcMacroServerChoice};
use ra_ap_project_model::CargoConfig;
use ra_ap_syntax::{AstNode, Edition, NodeOrToken, TextRange, WalkEvent};
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::ops::Index;
use std::path::{Path, PathBuf};

/// example module for testing
mod another {
    pub const MAX: usize = usize::MAX;
    pub struct X {
        field1: usize,
        field2: std::path::PathBuf,
    }
    pub enum EnumWithBodiedVariant {
        Variant = isize::MIN,
    }
    pub enum EnumWithFieldedVariant {
        Variant(X),
    }
    pub mod foo {
        pub const CONSTANT: &str = "someconstant";
        pub const OTHER_CONSTANT: &str = "otherconstant";
    }
    pub mod bar {
        pub const CONSTANT: &str = "barconst";
    }
}

#[allow(unused)]
fn returns_pathbuf() -> std::path::PathBuf {
    PathBuf::new() //unimplemented!()
}

#[allow(unused)]
fn synthetic_usages() {
    let _ = main;
    use another::bar;
    eprintln!("{}", another::foo::OTHER_CONSTANT);
    let c = another::foo::CONSTANT;
    let alsomain = crate::main;
    let indirect = bar::CONSTANT;
}

static ERR: std::io::ErrorKind = std::io::ErrorKind::NotFound;
static EDITION: Edition = Edition::Edition2021;

/// Analyze a Rust codebase to determine relations between items.
#[derive(Parser)]
struct Args {
    /// Directory of Rust project to analyze. `Cargo.toml` should reside inside this directory.
    cargo_dir_path: PathBuf,
    #[arg(required = true)]
    /// Paths to Rust item to analyze.
    item_paths: Vec<String>,
}

/// Obtain the textual range of a given item
fn module_def_source(
    sema: &Semantics<RootDatabase>,
    module_def: ModuleDef,
) -> Option<InFile<TextRange>> {
    fn source_text_range<Def: HasSource>(
        sema: &Semantics<RootDatabase>,
        def: Def,
    ) -> Option<InFile<TextRange>>
    where
        <Def as HasSource>::Ast: AstNode,
    {
        sema.source(def)
            .map(|infile_node| infile_node.map(|node| node.syntax().text_range()))
    }

    match module_def {
        ModuleDef::Module(m) => Some(m.definition_source_range(sema.db)),
        ModuleDef::Function(x) => source_text_range(sema, x),
        ModuleDef::Adt(x) => source_text_range(sema, x),
        ModuleDef::Variant(x) => source_text_range(sema, x),
        ModuleDef::Const(x) => source_text_range(sema, x),
        ModuleDef::Static(x) => source_text_range(sema, x),
        ModuleDef::Trait(x) => source_text_range(sema, x),
        ModuleDef::TypeAlias(x) => source_text_range(sema, x),
        ModuleDef::TraitAlias(x) => source_text_range(sema, x),
        ModuleDef::BuiltinType(_) => return None, // Builtins are not defined in source code
        ModuleDef::Macro(x) => source_text_range(sema, x),
    }
}

/// Visit all items mentioned by a type
fn type_visit_items(
    db: &dyn ra_ap_hir::db::HirDatabase,
    ty: ra_ap_hir::Type,
    mut callback: impl FnMut(ModuleDef),
) {
    log::debug!("walking ty {ty:?}");
    ty.walk(db, |ty| {
        log::debug!("walk encountered ty {ty:?}");
        if let Some(adt) = ty.as_adt() {
            log::trace!("adt!");
            callback(adt.into())
        } else if let Some(tr) = ty.as_dyn_trait() {
            log::trace!("dyn trait!");
            callback(tr.into())
        } else if let Some(_builtin) = ty.as_builtin() {
            // We may want to map these to libstd primitives or not...
        }
    });
}

/// Find all references to an item
fn item_uses(
    sema: &Semantics<RootDatabase>,
    items_by_range: &[(InFile<TextRange>, ModuleDef)],
    def: ModuleDef,
) -> HashSet<ModuleDef> {
    let def = Definition::from(def);
    let usages = def.usages(sema).include_self_refs().all();

    // Find the innermost item containing the text range of each usage occurrence
    let references = usages
        .into_iter()
        .flat_map(|(file_id, refs)| {
            refs.into_iter().filter_map(move |file_ref| {
                let range = match file_ref.name {
                    FileReferenceNode::Name(name) => name.syntax().text_range(),
                    FileReferenceNode::NameRef(name_ref) => name_ref.syntax().text_range(),
                    FileReferenceNode::Lifetime(_lt) => return None, // lifetimes will not reference items
                    FileReferenceNode::FormatStringEntry(_s, range) => range,
                };
                items_by_range
                    .iter()
                    .find(|(item_range, _item)| {
                        item_range.file_id == file_id && item_range.value.contains_range(range)
                    })
                    .map(|(_item_range, item)| item.clone())
            })
        })
        .collect();
    references
}

/// Return the function's signature as written in the source
///
/// Referenced paths will be expressed as written, which is usually not absolute and may require
/// scope context to resolve.
fn function_signature_as_written(sema: &Semantics<RootDatabase>, func: Function) -> Option<String> {
    let source_node = sema.source(func)?;
    let text = source_node.value.syntax().text();

    let Some(body) = source_node.value.body() else {
        return Some(text.to_string());
    };
    let end_pos = u32::from(text.len() - body.syntax().text_range().len());
    let range = TextRange::new(0.into(), end_pos.into());
    let mut signature = text.slice(range).to_string();

    // Drop any whitespace between signature and body
    signature.truncate(signature.trim_end().len());
    Some(signature)
}

/// Pretty-print a function signature to a string
fn pp_function_signature(db: &RootDatabase, sig: &FunctionSignature) -> String {
    let mut out = String::new();

    if let Some(abi) = &sig.abi {
        out.push_str("extern \"");
        out.push_str(abi.as_str());
        out.push_str("\" ");
    }

    let _ = write!(&mut out, "fn {}", sig.name.display(db, Edition::DEFAULT));

    if sig.generic_params.len() > 0 {
        out.push('<');
        for (_lt_id, lt_info) in sig.generic_params.iter_lt() {
            let _ = write!(&mut out, "{}, ", lt_info.name.display(db, Edition::DEFAULT));
        }
        for (_tc_id, tc_info) in sig.generic_params.iter_type_or_consts() {
            match tc_info {
                TypeOrConstParamData::TypeParamData(typaram) => {
                    if let Some(name) = &typaram.name {
                        let _ = write!(&mut out, "{}", name.display(db, Edition::DEFAULT));
                    } else {
                        out.push('_');
                    }
                    if let Some(_default) = typaram.default {
                        out.push_str(" = ");
                        //TODO: out.push_str(lookup_type_ref_id(default));
                        out.push_str("TODO");
                    }
                }
                TypeOrConstParamData::ConstParamData(constparam) => {
                    out.push_str("const ");
                    out.push_str(constparam.name.as_str());
                    out.push_str(": ");
                    //TODO: out.push_str(lookup_type_ref_id(constparam.ty));
                    out.push_str("TODO");
                    if let Some(_default) = constparam.default {
                        out.push_str(" = ");
                        //TODO: out.push_str(lookup_const_ref(default));
                        out.push_str("TODO");
                    }
                }
            }
            out.push_str(", ");
        }
        // remove trailing ", "
        out.pop();
        out.pop();
        out.push('>');
    }

    out.push('(');
    for param_ty_id in &sig.params {
        let ty = sig.store.index(*param_ty_id);
        let _ = write!(&mut out, "{:?}, ", ty);
    }
    if !sig.params.is_empty() {
        // remove trailing ", "
        out.pop();
        out.pop();
    }
    out.push(')');

    if let Some(ret_type) = &sig.ret_type {
        let ty = sig.store.index(*ret_type);
        let _ = write!(&mut out, " -> {:?}", ty);
    }

    out
}

/// Return the item in a module corresponding to a `Definition`.
///
/// Because `Definition` is used in "jump to definition", it covers a
/// variety of cases other than items, such as locals, labels, builtins,
/// etc.; this function returns `None` for these as they are not relevant
/// to the item dependency graph.
fn definition_source(d: Definition) -> Option<ModuleDef> {
    match d {
        Definition::Macro(x) => Some(ModuleDef::Macro(x)),
        Definition::Module(x) => Some(ModuleDef::Module(x)),
        Definition::Function(x) => Some(ModuleDef::Function(x)),
        Definition::Adt(x) => Some(ModuleDef::Adt(x)),
        Definition::Variant(x) => Some(ModuleDef::Variant(x)),
        Definition::Const(x) => Some(ModuleDef::Const(x)),
        Definition::Static(x) => Some(ModuleDef::Static(x)),
        Definition::Trait(x) => Some(ModuleDef::Trait(x)),
        Definition::TypeAlias(x) => Some(ModuleDef::TypeAlias(x)),
        Definition::BuiltinType(x) => Some(ModuleDef::BuiltinType(x)),
        _ => None,
    }
}

/// Prefer over `ModuleDef::canonical_path`.
pub fn canonical_item_path(
    item: &ModuleDef,
    db: &dyn ra_ap_hir::db::HirDatabase,
    edition: Edition,
) -> Option<String> {
    let mut segments = vec![item.name(db)?];
    // Include type before enum variants
    if let ModuleDef::Variant(variant) = item {
        segments.push(variant.parent_enum(db).name(db))
    }
    for m in item.module(db)?.path_to_root(db) {
        segments.extend(m.name(db))
    }
    segments.reverse();
    Some(segments.iter().map(|it| it.display(db, edition)).join("::"))
}

/// Generate a string representation of an item's full path.
fn absolute_item_path(db: &RootDatabase, item: ModuleDef, edition: Edition) -> String {
    let mut out = String::new();
    if let Some(name) = item.krate(db).display_name(db) {
        let _ = write!(&mut out, "::{}::", name.as_str());
    }
    if let Some(path) = item.canonical_module_path(db) {
        // This also adds any necessary trailing `::`
        append_module_path_to_string(db, path, edition, &mut out);
    }
    // Include type before enum variants
    if let ModuleDef::Variant(variant) = item {
        let _ = write!(
            &mut out,
            "{}::",
            variant.parent_enum(db).name(db).display(db, edition)
        );
    }
    if let Some(name) = item.name(db) {
        let _ = write!(&mut out, "{}", name.display(db, edition));
    } else {
        // If no name is present (e.g., this defn is a crate), remove trailing `::`.
        out.pop();
        out.pop();
    }
    out
}

/// Format each component of a path, followed by `::`, into the given `String`.
fn append_module_path_to_string(
    db: &RootDatabase,
    module_path: impl Iterator<Item = Module>,
    edition: Edition,
    out: &mut String,
) {
    for module in module_path {
        if let Some(name) = module.name(db) {
            let _ = write!(out, "{}::", name.display(db, edition));
        }
    }
}

/// Returns the paths of all items used by the given item.
fn items_used_by(sema: &Semantics<RootDatabase>, module_def: ModuleDef) -> HashSet<ModuleDef> {
    let mut used_module_defs: HashSet<_> = Default::default();
    let mut use_type_components = |ty| {
        type_visit_items(sema.db, ty, |item| {
            used_module_defs.insert(item);
        })
    };

    let syntax_node = match module_def {
        ModuleDef::Function(func) => {
            // iterate all params including self, as if we called this as an assoc fn
            for param in func.assoc_fn_params(sema.db) {
                use_type_components(param.ty().clone());
            }
            use_type_components(func.ret_type(sema.db));

            let fn_ast = sema
                .source::<Function>(func)
                .expect("def without source")
                .value;
            // Obtain body definition as token sequence
            fn_ast
                .body()
                .expect("function has no body")
                .syntax()
                .to_owned()
        }
        ModuleDef::Static(s) => {
            use_type_components(s.ty(sema.db));
            let static_ast = sema.source::<_>(s).expect("def without source").value;
            static_ast.body().expect("body").syntax().to_owned()
        }
        ModuleDef::Const(c) => {
            use_type_components(c.ty(sema.db));
            let const_ast = sema.source::<_>(c).expect("def without source").value;
            const_ast.body().expect("const value").syntax().to_owned()
        }
        ModuleDef::Variant(v) => {
            for f in v.fields(sema.db) {
                use_type_components(f.ty(sema.db))
            }
            let variant_ast = sema.source(v).expect("def without source").value;
            match variant_ast.expr() {
                Some(body) => body.syntax().to_owned(),
                None => return used_module_defs,
            }
        }
        ModuleDef::Module(_m) => return used_module_defs, // Code doesn't depend on modules directly, only their contained items.
        // We don't have a separate item type for struct/union field mentions, so consider their
        // fields' types to also be mentioned.
        ModuleDef::Adt(adt) => {
            match adt {
                Adt::Struct(s) => {
                    for f in s.fields(sema.db) {
                        use_type_components(f.ty(sema.db))
                    }
                }
                Adt::Union(u) => {
                    for f in u.fields(sema.db) {
                        use_type_components(f.ty(sema.db))
                    }
                }
                Adt::Enum(_e) => {}
            };
            return used_module_defs;
        }
        // References to a trait don't necessarily need to know about its full signature;
        // individual method references should cover the relevant portions.
        ModuleDef::Trait(_tr) => return used_module_defs,
        ModuleDef::TraitAlias(_tra) => return used_module_defs,
        ModuleDef::TypeAlias(ta) => {
            use_type_components(ta.ty(sema.db));
            return used_module_defs;
        }
        ModuleDef::BuiltinType(_bt) => return used_module_defs, /* builtin types do not reference other definitions */
        ModuleDef::Macro(_mac) => return used_module_defs, /* we don't need to handle macro definitions for now */
    };

    // Query which definition each token refers to
    for event in syntax_node.preorder_with_tokens() {
        if let WalkEvent::Enter(e) = event {
            match e {
                NodeOrToken::Node(_n) => {}
                NodeOrToken::Token(token) => {
                    if let Some(defn) = ra_ap_ide_db::helpers::get_definition(&sema, token) {
                        // Convert jump-to-defn `Definition` into item `ModuleDef`.
                        // Skips non-item definitions like locals, labels, builtins, etc.
                        if let Some(module_def) = definition_source(defn) {
                            used_module_defs.insert(module_def);
                        }
                    }
                }
            }
        }
    }

    used_module_defs
}

fn main() -> Result<(), String> {
    env_logger::init();

    let args = Args::parse();
    let cargo_dir_path = Path::new(&args.cargo_dir_path);

    let mut cargo_config = CargoConfig::default();
    cargo_config.sysroot = Some(ra_ap_project_model::RustLibSource::Discover);

    let load_cargo_config: LoadCargoConfig = LoadCargoConfig {
        load_out_dirs_from_check: true,
        with_proc_macro_server: ProcMacroServerChoice::Sysroot,
        prefill_caches: false,
    };

    let (db, vfs, _proc_macro_client) = ra_ap_load_cargo::load_workspace_at(
        cargo_dir_path,
        &cargo_config,
        &load_cargo_config,
        &|_msg| {},
    )
    .unwrap();

    log::info!("loaded crate");

    let cargo_dir_vfspath: ra_ap_vfs::VfsPath =
        ra_ap_vfs::AbsPathBuf::assert_utf8(cargo_dir_path.to_owned()).into();
    // Find the first file in `vfs` under the cargo dir, which we use to find the target crate
    let (first_file_id, _) = vfs
        .iter()
        .find(|(_id, path)| path.starts_with(&cargo_dir_vfspath))
        .expect("could not find first file in crate");

    let sema = Semantics::new(&db);

    let krate = sema.first_crate(first_file_id).unwrap();

    let mut files = Vec::new();
    for m in krate.modules(&db) {
        if let Some(editioned_file_id) = m.as_source_file_id(&db) {
            sema.parse(editioned_file_id);
            let file_id = editioned_file_id.file_id(&db);
            let vfs_path = vfs.file_path(file_id);
            if let Some(_path) = vfs_path.as_path() {
                files.push(editioned_file_id);
            }
        }
    }

    // Iterate all items, constructing map of their text ranges and finding any in `args.item_paths`
    let mut unfound_paths: HashSet<_> = args.item_paths.into_iter().collect();
    let mut found_items: HashMap<_, _> = Default::default();

    let mut items_by_range = Vec::new();
    for file in &files {
        let db = &db;
        let mut observe_def = |module_def: ModuleDef| {
            log::trace!(
                "traversal saw item {:?}",
                module_def.name(db).as_ref().map(Name::as_str)
            );

            // Mark this item as found if its path matches one we're looking for
            let abs_path = absolute_item_path(db, module_def, file.edition(db));
            let canonical_path = canonical_item_path(&module_def, db, file.edition(db));

            if let Some(path) = canonical_path
                .and_then(|path| unfound_paths.take(&path))
                .or_else(|| unfound_paths.take(&abs_path))
            {
                log::debug!("item traversal found queried path: {path}");
                found_items.insert(path.to_owned(), module_def);
            }

            // Save source range
            if let Some(text_range) = module_def_source(&sema, module_def) {
                log::trace!(
                    "range map: {text_range:?} -> {:?}",
                    module_def.name(sema.db)
                );
                items_by_range.push((text_range, module_def));
            }
        };
        ra_ap_ide_db::helpers::visit_file_defs(&sema, file.file_id(db), &mut |defn: Definition| {
            if let Some(module_def) = definition_source(defn) {
                // Despite its documentation, `visit_file_defs` does not visit enum variants
                if let ModuleDef::Adt(ra_ap_hir::Adt::Enum(e)) = module_def {
                    log::trace!("traversing enum");
                    for v in e.variants(db) {
                        observe_def(ModuleDef::Variant(v));
                    }
                }
                observe_def(module_def);
            }
        });
    }
    // Sort shorter ranges first, so that we examine inner items before their parents
    items_by_range.sort_by_key(|(text_range, _item)| text_range.value.len());

    for path in &unfound_paths {
        eprintln!("did not find item {path} in crate");
    }
    if !unfound_paths.is_empty() {
        return Err("not all paths were found in the crate".into());
    }

    let mut output = serde_json::Map::new();
    for (path, module_def) in found_items {
        let mut path_info = serde_json::Map::new();
        log::info!("processing {path}");
        // Find uses of the item
        {
            let using_items = item_uses(&sema, &items_by_range, module_def);
            let paths = using_items
                .into_iter()
                .map(|module_def| absolute_item_path(&db, module_def, Edition::DEFAULT))
                .collect::<Vec<_>>();
            path_info.insert("uses".to_owned(), paths.into());
        }
        // Find used items
        {
            let used_items = items_used_by(&sema, module_def);
            let paths = used_items
                .into_iter()
                .map(|module_def| absolute_item_path(&db, module_def, Edition::DEFAULT))
                .collect::<Vec<_>>();
            path_info.insert("used_items".to_owned(), paths.into());
        }
        // Output signature for functions
        {
            let id: Option<ModuleDefId> = module_def.try_into().ok();
            if let Some(ModuleDefId::FunctionId(func_id)) = id {
                let sig = db.function_signature(func_id);
                path_info.insert(
                    "signature".to_owned(),
                    pp_function_signature(&db, &*sig).into(),
                );
                path_info.insert(
                    "written_signature".to_owned(),
                    function_signature_as_written(&sema, func_id.into()).into(),
                );
            }
        }
        output.insert(path, path_info.into());
    }
    let mut stdout = std::io::stdout().lock();
    serde_json::to_writer(&mut stdout, &output)
        .map_err(|e| format!("error writing output: {e}"))?;

    Ok(())
}
