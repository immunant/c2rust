extern crate handlebars;

use std::path::{PathBuf};

use self::handlebars::Handlebars;
use super::serde_json::{json};

use super::TranspilerConfig;


pub fn emit_templates(tcfg: &TranspilerConfig) {

    let mut reg = Handlebars::new();
    let templ_dir = PathBuf::from("c2rust-transpile/src/templates");

    reg.register_templates_directory(".hbs", templ_dir).unwrap();

    let json = json!({
        "crate_name": "wazzup",
        "main_module": tcfg.main_file
    });
    println!("{}", reg.render("Cargo.toml", &json).unwrap());

}