use std::io::{self, BufRead, Write};
use std::str::FromStr;
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
            let line = encode_message(msg);
            out.write_all(line.as_bytes()).unwrap();
            out.flush().unwrap();
        }
    });

    thread::spawn(move || {
        let in_ = io::stdin();
        let mut in_ = in_.lock();

        let mut line = String::new();
        while let Ok(_) = in_.read_line(&mut line) {
            // Drop trailing '\n'
            let end = line.len() - 1;
            let msg = decode_message(&line[..end]).unwrap();
            line.clear();
            to_server.send(msg);
        }
    });

    client_send
}


fn encode_message(msg: ToClient) -> String {
    match msg {
        ToClient::MarkInfo { id, file, start_line, start_col,
                             end_line, end_col, labels } => {
            let mut s = format!("mark-info {} {} {} {} {} {}",
                    id, file, start_line, start_col, end_line, end_col);
            for l in labels {
                s.push_str(&format!(" {}", l));
            }
            s.push('\n');
            s
        },

        ToClient::NodeList { nodes } => {
            let mut s = String::new();
            s.push_str("node-list");
            for node in nodes {
                s.push_str(&format!(" {}", node)); 
            }
            s.push('\n');
            s
        },

        ToClient::GetBufferText { file } =>
            format!("get-buffer-text {}\n", file),

        ToClient::NewBufferText { file, content } =>
            format!("new-buffer-text {}\n{}\n.\n", file, content),
    }
}

fn decode_message(line: &str) -> Result<ToServer, String> {
    let mut parts = line.split(" ");

    let kind = match parts.next() {
        Some(x) => x,
        None => return Err(format!("expected message kind")),
    };

    macro_rules! get_conv {
        ($t:ty) => {
            match parts.next() {
                Some(x) => {
                    match <$t>::from_str(x) {
                        Ok(y) => y,
                        Err(e) => return Err(format!("error while parsing {}: {}",
                                                     stringify!($t), e)),
                    }
                },
                None => return Err(format!("expected {} before end of line",
                                           stringify!($t))),
            }
        };
    };

    Ok(match &kind as &str {
        "add-mark" => {
            ToServer::AddMark {
                file: get_conv!(String),
                line: get_conv!(u32),
                col: get_conv!(u32),
                kind: get_conv!(String),
                label: get_conv!(String),
            }
        },

        "remove-mark" => {
            ToServer::RemoveMark {
                id: get_conv!(usize),
            }
        },

        "get-mark-info" => {
            ToServer::GetMarkInfo {
                id: get_conv!(usize),
            }
        },

        "get-node-list" => {
            ToServer::GetNodeList
        },

        "set-buffers-available" => {
            ToServer::SetBuffersAvailable {
                files: parts.map(|s| s.to_owned()).collect(),
            }
        },

        "buffer-text" => {
            ToServer::BufferText {
                file: get_conv!(String),
                content: get_conv!(String),
            }
        },

        "run-command" => {
            ToServer::RunCommand {
                name: get_conv!(String),
                args: parts.map(|s| s.to_owned()).collect(),
            }
        },

        s => return Err(format!("unrecognized message kind `{}`", s)),
    })
}
