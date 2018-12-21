#![feature(rustc_private)]
#![feature(label_break_value)]
extern crate serde_cbor;
extern crate syntax;
extern crate syntax_pos;
extern crate rustc_target;
extern crate dtoa;
#[macro_use] extern crate indexmap;
extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate serde_json;
extern crate libc;
extern crate clap;
extern crate c2rust_ast_exporter;
extern crate c2rust_ast_builder;
extern crate itertools;

use std::io::stdout;
use std::io::prelude::*;
use std::error::Error;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process;

use c_ast::*;
use c_ast::Printer;
use c2rust_ast_exporter as ast_exporter;

pub mod renamer;
pub mod convert_type;
pub mod build_files;
pub mod translator;
pub mod c_ast;
pub mod rust_ast;
pub mod cfg;
pub mod with_stmts;

pub use translator::ReplaceMode;
use build_files::emit_build_files;
use std::prelude::v1::Vec;

/// Configuration settings for the translation process
#[derive(Debug)]
pub struct TranspilerConfig {
    // Debug output options
    pub dump_untyped_context: bool,
    pub dump_typed_context: bool,
    pub pretty_typed_context: bool,
    pub dump_function_cfgs: bool,
    pub json_function_cfgs: bool,
    pub dump_cfg_liveness: bool,
    pub dump_structures: bool,
    // Options that control translation
    pub incremental_relooper: bool,
    pub fail_on_multiple: bool,
    pub debug_relooper_labels: bool,
    pub cross_checks: bool,
    pub cross_check_configs: Vec<String>,
    pub prefix_function_names: Option<String>,
    pub translate_asm: bool,
    pub use_c_loop_info: bool,
    pub use_c_multiple_info: bool,
    pub simplify_structures: bool,
    pub panic_on_translator_failure: bool,
    pub emit_modules: bool,
    pub fail_on_error: bool,
    pub replace_unsupported_decls: ReplaceMode,
    pub translate_valist: bool,
    pub reduce_type_annotations: bool,
    pub reorganize_definitions: bool,


    // Options that control build files
    /// Emit `Cargo.toml` and one of `main.rs`, `lib.rs`
    pub emit_build_files: bool,
    /// Names the translation unit containing the main function
    pub main: Option<String>,
    /// Use log-based cross checking
    pub use_fakechecks: bool,
}

/// Main entry point to transpiler. Called from CLI tools with the result of
/// clap::App::get_matches().
pub fn transpile(tcfg: TranspilerConfig, cc_db: &Path, extra_clang_args: &[&str]) {

    // TODO: bindgen may have a more elegant solution to this issue
    // MacOS Mojave does not have `/usr/include` even if Xcode or the
    // command line developer tools are installed.
    // See https://forums.developer.apple.com/thread/104296
    if cfg!(target_os = "macos") {
        let usr_incl = Path::new("/usr/include");
        if !usr_incl.exists() {
            eprintln!("
Directory `/usr/include` was not found! Please install the following package:
/Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg
 (or the equivalent version on your host.)");
            return
        }
    }

    let cmds = get_compile_commands(cc_db).unwrap();
    let mut modules = Vec::<PathBuf>::new();
    for mut cmd in cmds {
        match cmd {
            CompileCmd { directory: d, file: f, command: None, arguments: _, output: None} => {
                println!("transpiling {}", f.to_str().unwrap());
                let input_file_abs = d.join(f);

                let m = transpile_single(&tcfg, input_file_abs.as_path(), cc_db, extra_clang_args);
                modules.push(m);
            },
            _ => {
                let reason = format!("unhandled compile cmd: {:?}", cmd);
                panic!(reason);
            }
        }
   }

   if tcfg.emit_build_files {
       emit_build_files(&tcfg, cc_db, modules)
   }
}

#[derive(Deserialize, Debug)]
struct CompileCmd {
    /// The working directory of the compilation. All paths specified in the command
    /// or file fields must be either absolute or relative to this directory.
    directory: PathBuf,
    /// The main translation unit source processed by this compilation step. This is
    /// used by tools as the key into the compilation database. There can be multiple
    /// command objects for the same file, for example if the same source file is compiled
    /// with different configurations.
    file: PathBuf,
    /// The compile command executed. After JSON unescaping, this must be a valid command
    /// to rerun the exact compilation step for the translation unit in the environment
    /// the build system uses. Parameters use shell quoting and shell escaping of quotes,
    /// with ‘"’ and ‘\’ being the only special characters. Shell expansion is not supported.
    command: Option<String>,
    /// The compile command executed as list of strings. Either arguments or command is required.
    arguments: Vec<String>,
    /// The name of the output created by this compilation step. This field is optional. It can
    /// be used to distinguish different processing modes of the same input file.
    output: Option<String>,
}

fn get_compile_commands(compile_commands: &Path) -> Result<Vec<CompileCmd>, Box<Error>> {
    let f = File::open(compile_commands)?; // open read-only

    // Read the JSON contents of the file as an instance of `Value`
    let v = serde_json::from_reader(f)?;

    Ok(v)
}

fn transpile_single(tcfg: &TranspilerConfig, input_path: &Path, cc_db: &Path, extra_clang_args: &[&str])
    -> PathBuf {
    // Extract the untyped AST from the CBOR file
    let untyped_context = match ast_exporter::get_untyped_ast(input_path, cc_db, extra_clang_args) {
        Err(e) => {
            eprintln!("Error: {:}", e);
            process::exit(1);
        }
        Ok(cxt) => cxt,
    };

    if tcfg.dump_untyped_context {
        println!("CBOR Clang AST");
        println!("{:#?}", untyped_context);
    }

    // Convert this into a typed AST
    let typed_context = {
        let mut conv = ConversionContext::new(&untyped_context);
        conv.convert(&untyped_context);
        conv.typed_context
    };

    if tcfg.dump_typed_context {
        println!("Clang AST");
        println!("{:#?}", typed_context);
    }

    if tcfg.pretty_typed_context {
        println!("Pretty-printed Clang AST");
        println!("{:#?}", Printer::new(stdout()).print(&typed_context));
    }

    // Perform the translation
    let main_file = input_path.with_extension("");
    let translated_string = translator::translate(typed_context, &tcfg, main_file);
    let output_path = get_output_path(input_path);

    let mut file = match File::create(&output_path) {
        Ok(file) => file,
        Err(e) => panic!("Unable to open file for writing: {}", e),
    };

    match file.write_all(translated_string.as_bytes()) {
        Ok(()) => (),
        Err(e) => panic!("Unable to write translation to file: {}", e),
    };

    output_path
}

fn get_output_path(input_path: &Path) -> PathBuf {
    let mut path_buf = PathBuf::from(input_path);

    // When an output file name is not explictly specified, we should convert files
    // with dashes to underscores, as they are not allowed in rust file names.
    let file_name = path_buf.file_name().unwrap().to_str().unwrap().replace('-', "_");

    path_buf.set_file_name(file_name);
    path_buf.set_extension("rs");
    path_buf
}
