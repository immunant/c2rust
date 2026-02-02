use clap::Parser;
use ra_ap_hir::{
    DefWithBody, EditionedFileId, Function, HasCrate, HasSource, InFile, Module, ModuleDef,
    ModuleDefId, Name, Semantics, db::DefDatabase,
};
use ra_ap_hir_def::{hir::generics::TypeOrConstParamData, signatures::FunctionSignature};
use ra_ap_ide_db::search::FileReferenceNode;
use ra_ap_ide_db::{FileId, RootDatabase, defs::Definition};
use ra_ap_load_cargo::{self, LoadCargoConfig, ProcMacroServerChoice};
use ra_ap_project_model::CargoConfig;
use ra_ap_syntax::{AstNode, Edition, NodeOrToken, TextRange, WalkEvent};
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::ops::Index;
use std::path::{Path, PathBuf};

/// example module for testing
mod another {
    pub mod foo {
        pub const CONSTANT: &str = "someconstant";
        pub const OTHER_CONSTANT: &str = "otherconstant";
    }
    pub mod bar {
        pub const CONSTANT: &str = "barconst";
    }
}

#[allow(unused)]
fn synthetic_usages() {
    let _ = main;
    use another::bar;
    eprintln!("{}", another::foo::OTHER_CONSTANT); //XXX: usage is not found here
    let c = another::foo::CONSTANT;
    let alsomain = crate::main;
    let indirect = bar::CONSTANT;
}

enum Query {
    Uses,
    UsedItems,
    FnSignature,
}

/// Analyze a Rust codebase to determine relations between items.
#[derive(Parser)]
struct Args {
    /// Directory of Rust project to analyze. `Cargo.toml` should reside inside this directory.
    cargo_dir_path: PathBuf,
    // /// Kind of relationships to obtain
    // mode: Query,
    /// Path to Rust item to start from.
    item_path: String,
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

/// Find all references to an item
fn item_uses(
    sema: &Semantics<RootDatabase>,
    items_by_range: &[(InFile<TextRange>, ModuleDef)],
    def: Definition,
) -> HashSet<ModuleDef> {
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
    let dwb = module_def.as_def_with_body().unwrap();
    let func = match dwb {
        DefWithBody::Function(f) => f,
        _ => panic!("not function!"),
    };

    // Obtain body definition as token sequence
    let fn_ast = sema.source::<Function>(func).expect("def no source").value;
    let body_ast = fn_ast.body().expect("function has no body");
    let syntax_node = body_ast.syntax();

    // Query which definition each token refers to
    let mut used_defns: HashSet<_> = Default::default();
    for event in syntax_node.preorder_with_tokens() {
        if let WalkEvent::Enter(e) = event {
            match e {
                NodeOrToken::Node(_n) => {}
                NodeOrToken::Token(token) => {
                    if let Some(defn) = ra_ap_ide_db::helpers::get_definition(&sema, token) {
                        used_defns.insert(defn);
                    }
                }
            }
        }
    }

    // Convert jump-to-defn `Definition`s into item `ModuleDef`s
    used_defns
        .into_iter()
        .filter_map(|decl| {
            // Skip non-item definitions like locals, labels, builtins, etc.
            definition_source(decl)
        })
        .collect()
}

/// Find definitions in this file of any items whose canonical paths are in `to_find`.
///
/// Inserts found definitions into the `found_items` map.
fn find_items(
    db: &RootDatabase,
    sema: &Semantics<RootDatabase>,
    file_id: EditionedFileId,
    to_find: &mut HashSet<String>,
    found_items: &mut HashMap<String, ModuleDef>,
    edition: Edition,
) {
    // Recursive helper to search a single module
    fn find_module_items(
        db: &RootDatabase,
        module: Module,
        to_find: &mut HashSet<String>,
        found_items: &mut HashMap<String, ModuleDef>,
        edition: Edition,
    ) {
        for decl in module.declarations(db) {
            log::trace!(
                "traversal saw item {:?}",
                decl.name(db).as_ref().map(Name::as_str)
            );
            to_find.retain(|path| {
                if decl.canonical_path(db, edition).as_ref() == Some(path)
                    || absolute_item_path(db, decl, edition) == *path
                {
                    log::debug!("item traversal found queried path: {path}");
                    found_items.insert(path.to_owned(), decl);
                    return false;
                }
                true
            })
        }
        for child_mod in module.children(db) {
            find_module_items(db, child_mod, to_find, found_items, edition);
        }
    }

    for module_def in sema.hir_file_to_module_defs(file_id.clone()) {
        find_module_items(db, module_def, to_find, found_items, edition);
    }
}

fn main() -> Result<(), ()> {
    env_logger::init();

    let args = Args::parse();
    let cargo_dir_path = Path::new(&args.cargo_dir_path);

    let cargo_config = CargoConfig::default();

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

    // Assume the first file in `vfs` is the crate root.
    let (first_file_id, _) = vfs.iter().next().unwrap();

    let sema = Semantics::new(&db);

    let krate = sema.first_crate(first_file_id).unwrap();

    let mut files = Vec::new();
    for m in krate.modules(&db) {
        let src = m.definition_source(&db);
        let node = src.value.node();
        if let Some(editioned_file_id) = m.as_source_file_id(&db) {
            sema.parse(editioned_file_id);
            let file_id = editioned_file_id.file_id(&db);
            let vfs_path = vfs.file_path(file_id);
            if let Some(path) = vfs_path.as_path() {
                files.push((editioned_file_id, path.to_path_buf(), node));
            }
        }
    }

    // Construct map of the text ranges of each item in every file
    let file_ids: Vec<FileId> = files.iter().map(|(id, _, _)| id.file_id(&db)).collect();
    let mut items_by_range = Vec::new();
    for &file_id in &file_ids {
        ra_ap_ide_db::helpers::visit_file_defs(&sema, file_id, &mut |defn: Definition| {
            if let Some(module_def) = definition_source(defn) {
                if let Some(text_range) = module_def_source(&sema, module_def) {
                    log::trace!(
                        "range map: {text_range:?} -> {:?}",
                        module_def.name(sema.db)
                    );
                    items_by_range.push((text_range, module_def));
                }
            }
        });
    }
    // Sort shorter ranges first, so that we examine inner items before their parents
    items_by_range.sort_by_key(|(text_range, _item)| text_range.value.len());

    // Currently we accept a single item path, but to amortize indexing
    // we may want to accept multiple ones later
    let mut unfound_paths: HashSet<_> = [args.item_path.clone()].into_iter().collect();
    let mut found_items: HashMap<_, _> = Default::default();
    for (file_id, _, _) in &files {
        find_items(
            &db,
            &sema,
            file_id.clone(),
            &mut unfound_paths,
            &mut found_items,
            Edition::DEFAULT,
        );
    }

    for path in &unfound_paths {
        eprintln!("did not find item {path} in crate");
    }
    if !unfound_paths.is_empty() {
        return Err(());
    }

    println!("{{");
    for (path, module_def) in found_items {
        log::info!("processing {path}");
        println!("\"{path}\": {{");
        let mut first = true;
        for query in [Query::Uses, Query::UsedItems, Query::FnSignature] {
            if !first {
                print!(",");
            } else {
                first = false;
            }
            match query {
                Query::Uses => {
                    let def = Definition::try_from(module_def).expect(&format!(
                        "could not convert `ModuleDef` to `Definition` for {path}"
                    ));
                    let using_items = item_uses(&sema, &items_by_range, def);
                    let paths = using_items
                        .into_iter()
                        .map(|module_def| absolute_item_path(&db, module_def, Edition::DEFAULT))
                        .collect::<Vec<_>>();
                    println!("\"uses\": {paths:?}");
                }
                Query::UsedItems => {
                    let used_items = items_used_by(&sema, module_def);
                    let paths = used_items
                        .into_iter()
                        .map(|module_def| absolute_item_path(&db, module_def, Edition::DEFAULT))
                        .collect::<Vec<_>>();
                    println!("\"used_items\": {paths:?}");
                }
                Query::FnSignature => {
                    let id: Option<ModuleDefId> = module_def.try_into().ok();
                    if let Some(ModuleDefId::FunctionId(func_id)) = id {
                        let sig = db.function_signature(func_id);
                        println!("\"signature\": {:?}", pp_function_signature(&db, &*sig));
                    }
                }
            }
        }
        println!("}}");
    }
    println!("}}");

    Ok(())
}
