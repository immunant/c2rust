use std::io::{self, BufRead, Write};
use std::sync::mpsc::{self, Sender};
use std::thread;
use json::{self, JsonValue};

use interact::{ToServer, ToClient};
use interact::WrapSender;


pub fn init<U, F>(to_server: WrapSender<ToServer, U, F>) -> Sender<ToClient>
        where U: Send + 'static,
              F: Fn(ToServer) -> U + Send + 'static {
    let (client_send, client_recv) = mpsc::channel();

    thread::spawn(move || {
        let out = io::stdout();
        let mut out = out.lock();

        for msg in client_recv.iter() {
            info!("sending: {:?}", msg);
            let json = encode_message(msg);
            json.write(&mut out).unwrap();
            out.write_all(b"\n").unwrap();
            out.flush().unwrap();
        }
    });

    thread::spawn(move || {
        let in_ = io::stdin();
        let mut in_ = in_.lock();

        let mut line = String::new();
        while let Ok(_) = in_.read_line(&mut line) {
            let json = json::parse(&line).unwrap();
            let msg = decode_message(json).unwrap();
            info!("received: {:?}", msg);
            line.clear();
            to_server.send(msg).unwrap();
        }
    });

    client_send
}


fn encode_message(msg: ToClient) -> JsonValue {
    match msg {
        ToClient::MarkInfo { id, file, start_line, start_col,
                             end_line, end_col, labels } => {
            object! {
                "msg" => "mark-info",
                "id" => id,
                "file" => file,
                "start_line" => start_line,
                "start_col" => start_col,
                "end_line" => end_line,
                "end_col" => end_col,
                "labels" => labels
            }
        },

        ToClient::NodeList { nodes } => {
            object! {
                "msg" => "node-list",
                "nodes" => nodes
            }
        },

        ToClient::GetBufferText { file } => {
            object! {
                "msg" => "get-buffer-text",
                "file" => file
            }
        },

        ToClient::NewBufferText { file, content } => {
            object! {
                "msg" => "new-buffer-text",
                "file" => file,
                "content" => content
            }
        },

        ToClient::Error { text } => {
            object! {
                "msg" => "error",
                "text" => text
            }
        },
    }
}

fn decode_message(json: JsonValue) -> Result<ToServer, String> {
    let mut obj = match json {
        JsonValue::Object(obj) => obj,
        _ => return Err("expected object".to_owned()),
    };

    macro_rules! get_conv {
        ($json:expr, $key:expr, $conv:ident) => {
            match $json.get_mut($key) {
                Some(x) => match x.$conv() {
                    Some(y) => y,
                    None => return Err(format!("conversion `{}` failed on key `{}`",
                                               stringify!($conv), $key)),
                },
                None => return Err(format!("missing key `{}`", $key)),
            }
        };
    };

    macro_rules! get_conv_array {
        ($json:expr, $key:expr, $conv:ident) => {
            {
                let val = match $json.get_mut($key) {
                    Some(x) => x,
                    None => return Err(format!("missing key `{}`", $key)),
                };
                let arr = match *val {
                    JsonValue::Array(ref mut x) => x,
                    _ => return Err(format!("expected key `{}` to contain an array", $key)),
                };

                let mut result = Vec::with_capacity(arr.len());
                for (i, x) in arr.iter_mut().enumerate() {
                    match x.$conv() {
                        Some(y) => result.push(y),
                        None => return Err(format!(
                                "conversion `{}` failed on element {} of array `{}`",
                                stringify!($conv), i, $key)),
                    }
                }

                result
            }
        };
    };

    let kind = get_conv!(obj, "msg", take_string);

    Ok(match &kind as &str {
        "add-mark" => {
            ToServer::AddMark {
                file: get_conv!(obj, "file", take_string),
                line: get_conv!(obj, "line", as_u32),
                col: get_conv!(obj, "col", as_u32),
                kind: get_conv!(obj, "kind", take_string),
                label: get_conv!(obj, "label", take_string),
            }
        },

        "remove-mark" => {
            ToServer::RemoveMark {
                id: get_conv!(obj, "id", as_usize),
            }
        },

        "get-mark-info" => {
            ToServer::GetMarkInfo {
                id: get_conv!(obj, "id", as_usize),
            }
        },

        "get-node-list" => {
            ToServer::GetNodeList
        },

        "set-buffers-available" => {
            ToServer::SetBuffersAvailable {
                files: get_conv_array!(obj, "files", take_string),
            }
        },

        "buffer-text" => {
            ToServer::BufferText {
                file: get_conv!(obj, "file", take_string),
                content: get_conv!(obj, "content", take_string),
            }
        },

        "run-command" => {
            ToServer::RunCommand {
                name: get_conv!(obj, "name", take_string),
                args: get_conv_array!(obj, "args", take_string),
            }
        },

        s => return Err(format!("unrecognized message kind `{}`", s)),
    })
}
