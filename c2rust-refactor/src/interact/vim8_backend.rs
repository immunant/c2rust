//! JSON backend, for communication with Vim 8.
use json::{self, object, JsonValue};
use log::info;
use std::io::{self, BufRead, Write};
use std::sync::mpsc::{self, SyncSender};
use std::thread;

use crate::interact::WrapSender;
use crate::interact::{MarkInfo, ToClient, ToServer};

pub fn init<U, F>(to_server: WrapSender<ToServer, U, F>) -> SyncSender<ToClient>
where
    U: Send + 'static,
    F: Fn(ToServer) -> U + Send + 'static,
{
    let (client_send, client_recv) = mpsc::sync_channel(1);

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

fn encode_mark_info(i: MarkInfo) -> JsonValue {
    object! {
        "id" => i.id,
        "file" => i.file,
        "start_line" => i.start_line,
        "start_col" => i.start_col,
        "end_line" => i.end_line,
        "end_col" => i.end_col,
        "labels" => i.labels
    }
}

fn encode_message(msg: ToClient) -> JsonValue {
    match msg {
        ToClient::Mark { info } => {
            object! {
                "msg" => "mark",
                "info" => encode_mark_info(info)
            }
        }

        ToClient::MarkList { infos } => {
            object! {
                "msg" => "mark-list",
                "infos" => infos.into_iter().map(encode_mark_info).collect::<Vec<_>>()
            }
        }

        ToClient::GetBufferText { file } => {
            object! {
                "msg" => "get-buffer-text",
                "file" => file
            }
        }

        ToClient::NewBufferText { file, content } => {
            object! {
                "msg" => "new-buffer-text",
                "file" => file,
                "content" => content
            }
        }

        ToClient::Error { text } => {
            object! {
                "msg" => "error",
                "text" => text
            }
        }
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
                    None => {
                        return Err(format!(
                            "conversion `{}` failed on key `{}`",
                            stringify!($conv),
                            $key
                        ))
                    }
                },
                None => return Err(format!("missing key `{}`", $key)),
            }
        };
    }

    macro_rules! get_conv_array {
        ($json:expr, $key:expr, $conv:ident) => {{
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
                    None => {
                        return Err(format!(
                            "conversion `{}` failed on element {} of array `{}`",
                            stringify!($conv),
                            i,
                            $key
                        ))
                    }
                }
            }

            result
        }};
    }

    let kind = get_conv!(obj, "msg", take_string);

    Ok(match &kind as &str {
        "add-mark" => ToServer::AddMark {
            file: get_conv!(obj, "file", take_string),
            line: get_conv!(obj, "line", as_u32),
            col: get_conv!(obj, "col", as_u32),
            kind: get_conv!(obj, "kind", take_string),
            label: get_conv!(obj, "label", take_string),
        },

        "remove-mark" => ToServer::RemoveMark {
            id: get_conv!(obj, "id", as_usize),
        },

        "get-mark-info" => ToServer::GetMarkInfo {
            id: get_conv!(obj, "id", as_usize),
        },

        "get-mark-list" => ToServer::GetMarkList,

        "set-buffers-available" => ToServer::SetBuffersAvailable {
            files: get_conv_array!(obj, "files", take_string),
        },

        "buffer-text" => ToServer::BufferText {
            file: get_conv!(obj, "file", take_string),
            content: get_conv!(obj, "content", take_string),
        },

        "run-command" => ToServer::RunCommand {
            name: get_conv!(obj, "name", take_string),
            args: get_conv_array!(obj, "args", take_string),
        },

        s => return Err(format!("unrecognized message kind `{}`", s)),
    })
}
