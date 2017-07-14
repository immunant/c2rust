use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::{self, BufRead};
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::mpsc::{self, Sender, Receiver};
use std::thread;
use json::{self, JsonValue};
use syntax::ast::{NodeId, Crate};
use syntax::codemap::{FileLoader, RealFileLoader};
use syntax::symbol::Symbol;

use command::{self, CommandState};
use driver;
use file_rewrite::{self, RewriteMode};
use interact::{ToServer, ToClient};
use interact::WrapSender;
use interact::{plain_backend, vim8_backend};
use pick_node;
use rewrite;
use span_fix;
use util::IntoSymbol;


pub enum ToWorker {
    InputMessage(ToServer),
    NeedFile(PathBuf, Sender<String>),
}

struct WorkerState {
    to_client: Sender<ToClient>,
    to_main: Sender<ToServer>,

    pending_files: HashMap<PathBuf, Sender<String>>,
}

impl WorkerState {
    fn new(to_client: Sender<ToClient>,
           to_main: Sender<ToServer>) -> WorkerState {
        WorkerState {
            to_client: to_client,
            to_main: to_main,

            pending_files: HashMap::new(),
        }
    }

    fn run_loop(&mut self,
                worker_recv: Receiver<ToWorker>) {
        for msg in worker_recv.iter() {
            self.handle_one(msg);
        }
    }

    fn handle_one(&mut self, msg: ToWorker) {
        use self::ToWorker::*;
        use super::ToServer::*;
        use super::ToClient::*;

        match msg {
            InputMessage(BufferText { file, content }) => {
                info!("got text for file {:?}", file);
                let path = fs::canonicalize(&file).unwrap();
                let send = match self.pending_files.remove(&path) {
                    Some(x) => x,
                    None => {
                        warn!("got file {:?}, but no request for it is pending", path);
                        return;
                    },
                };
                send.send(content).unwrap();
            },

            NeedFile(path, send) => {
                info!("got request for file {:?}", path);
                assert!(!self.pending_files.contains_key(&path));
                self.to_client.send(GetBufferText {
                    file: path.to_string_lossy().into_owned(),
                }).unwrap();
                self.pending_files.insert(path, send);
            },

            // Other messages pass through to the main thread.
            InputMessage(msg) => {
                self.to_main.send(msg).unwrap();
            },
        }
    }
}

pub fn run_worker(recv: Receiver<ToWorker>,
                  to_client: Sender<ToClient>,
                  to_main: Sender<ToServer>) {
    let mut state = WorkerState::new(to_client, to_main);
    state.run_loop(recv);
}
