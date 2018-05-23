#[macro_use]
extern crate rouille;
extern crate tempfile;

#[macro_use]
extern crate serde_derive;

use rouille::Request;
use rouille::Response;
use std::fs::File;
use std::path::Path;
use std::io::{self,Read,Write};
use std::process::Command;
use tempfile::tempdir;
use std::str;

#[derive(Serialize)]
struct TranslateErr<'a> {
    description: &'a str,
}

fn get_file(content_type: &'static str, path: &Path, filename: &Path) -> Response {

    let index_html = match File::open(path.join(filename)) {
        Ok(x) => x,
        Err(e) => return Response::text(format!("{:?}", e)).with_status_code(500),
    };

    Response::from_file(content_type, index_html)
}

fn translate(request: &Request) -> Response {
    let input = try_or_400!(post_input!(request, { src: String }));

    let dir = try_or_400!(tempdir());
    let c_path = dir.path().join("source.c");
    let json_path = dir.path().join("compile_commands.json");

    {
        let mut c_file = try_or_400!(File::create(&c_path));
        try_or_400!(c_file.write_all(input.src.as_bytes()));
    }

    {
        let mut json_file = try_or_400!(File::create(&json_path));
        try_or_400!(write!(json_file,
        "[{{\n\
            \"directory\": \"{}\",\n\
            \"command\": \"cc -o dummy.c.o -c {} -Wwrite-strings -D_FORTIFY_SOURCE=0 -DNDEBUG=1\",\n\
            \"file\": \"{}\"\n\
            }}]\n",
            dir.path().to_str().unwrap(),
            c_path.to_str().unwrap(),
            c_path.to_str().unwrap(),
        ));
    }

    let output = try_or_400!(
        Command::new("sh")
            .arg("-c")
            .arg(format!("../scripts/transpile.py {}", json_path.to_str().unwrap()))
            .output());

    if !output.status.success() {
        // these errors can really stack up
        let error_string = try_or_400!(str::from_utf8(&output.stderr[..]));

        return Response::json(&TranslateErr { description: error_string })
            .with_status_code(400)
    }

    let mut contents = String::new();
    {
        let output_path = dir.path().join("source.rs");
        let mut output_file = try_or_400!(File::open(output_path));
        try_or_400!(output_file.read_to_string(&mut contents));
    }

    Response::text(contents)
}

fn my_router(request: &Request) -> Response {

    // For whatever reason log doesn't bother recording the remote address
    print!("{} ", request.remote_addr());

    rouille::log(request, io::stdout(), || {
        router!(request,
        (GET) (/) => {
            get_file("text/html", Path::new("webroot"), Path::new("index.html"))
        },
        (GET) (/static/{_filename: String}) => {
            rouille::match_assets(&request, "webroot")
        },
        (GET) (/ace/{filename: String}) => {
            get_file("text/javascript", Path::new("webroot/ace"), Path::new(&filename))
        },
        (POST) (/translate) => { translate(request) },
        _ => { Response::empty_404() },
    )})
}

fn main() {
    rouille::start_server(":::8000", my_router);
}
