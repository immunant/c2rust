//! Plain-text backend, for testing interactive mode.
use std::io::{self, BufRead, Write};
use std::str::FromStr;
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
            to_server.send(msg).unwrap();
        }
    });

    client_send
}

fn encode_mark_info(i: MarkInfo) -> String {
    let mut s = format!(
        "info {} {} {} {} {} {} {}",
        i.id,
        i.file,
        i.start_line,
        i.start_col,
        i.end_line,
        i.end_col,
        i.labels.len()
    );
    for l in i.labels {
        s.push_str(&format!(" {}", l));
    }
    s
}

fn encode_message(msg: ToClient) -> String {
    match msg {
        ToClient::Mark { info } => format!("mark {}\n", encode_mark_info(info)),

        ToClient::MarkList { infos } => {
            let mut s = String::new();
            s.push_str("mark-list");
            for info in infos {
                s.push_str(&format!(" {}", encode_mark_info(info)));
            }
            s.push('\n');
            s
        }

        ToClient::GetBufferText { file } => format!("get-buffer-text {}\n", file),

        ToClient::NewBufferText { file, content } => {
            format!("new-buffer-text {}\n{}\n.\n", file, content)
        }

        ToClient::Error { text } => format!("error {}", text),
    }
}

#[allow(unreachable_code)]
fn decode_message(line: &str) -> Result<ToServer, String> {
    let mut parts = line.split(" ");

    let kind = match parts.next() {
        Some(x) => x,
        None => return Err(format!("expected message kind")),
    };

    macro_rules! get_conv {
        ($t:ty) => {
            match parts.next() {
                Some(x) => match <$t>::from_str(x) {
                    Ok(y) => y,
                    Err(e) => {
                        return Err(format!("error while parsing {}: {:?}", stringify!($t), e))
                    }
                },
                None => return Err(format!("expected {} before end of line", stringify!($t))),
            }
        };
    }

    Ok(match &kind as &str {
        "add-mark" => ToServer::AddMark {
            file: get_conv!(String),
            line: get_conv!(u32),
            col: get_conv!(u32),
            kind: get_conv!(String),
            label: get_conv!(String),
        },

        "remove-mark" => ToServer::RemoveMark {
            id: get_conv!(usize),
        },

        "get-mark-info" => ToServer::GetMarkInfo {
            id: get_conv!(usize),
        },

        "get-mark-list" => ToServer::GetMarkList,

        "set-buffers-available" => ToServer::SetBuffersAvailable {
            files: parts.map(|s| s.to_owned()).collect(),
        },

        "buffer-text" => ToServer::BufferText {
            file: get_conv!(String),
            content: get_conv!(String),
        },

        "run-command" => ToServer::RunCommand {
            name: get_conv!(String),
            args: parts.map(|s| s.to_owned()).collect(),
        },

        s => return Err(format!("unrecognized message kind `{}`", s)),
    })
}
