use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::mpsc::{Sender, Receiver};

use interact::{ToServer, ToClient};


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
