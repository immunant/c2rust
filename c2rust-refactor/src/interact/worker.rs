//! Worker thread implementation.
//!
//! The worker thread's main job is to hide the asynchrony of file loading from the main thread.
//! The interactive mode's custom `FileLoader` has to provide file contents synchronously, but
//! actually, obtaining file contents requires sending a request to the client and processing
//! messages until we get a response.  That loop happens in the worker thread.
use log::{info, warn};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::mpsc::{Receiver, Sender, SyncSender};

use crate::interact::{ToClient, ToServer};

pub enum ToWorker {
    InputMessage(ToServer),
    NeedFile(PathBuf, SyncSender<String>),
}

struct WorkerState {
    to_client: SyncSender<ToClient>,
    to_main: Sender<ToServer>,

    pending_files: HashMap<PathBuf, SyncSender<String>>,
}

impl WorkerState {
    fn new(to_client: SyncSender<ToClient>, to_main: Sender<ToServer>) -> WorkerState {
        WorkerState {
            to_client,
            to_main,

            pending_files: HashMap::new(),
        }
    }

    fn run_loop(&mut self, worker_recv: Receiver<ToWorker>) {
        for msg in worker_recv.iter() {
            self.handle_one(msg);
        }
    }

    fn handle_one(&mut self, msg: ToWorker) {
        use self::ToWorker::*;
        use super::ToClient::*;
        use super::ToServer::*;

        match msg {
            InputMessage(BufferText { file, content }) => {
                info!("got text for file {:?}", file);
                let path = fs::canonicalize(&file).unwrap();
                let send = match self.pending_files.remove(&path) {
                    Some(x) => x,
                    None => {
                        warn!("got file {:?}, but no request for it is pending", path);
                        return;
                    }
                };
                send.send(content).unwrap();
            }

            NeedFile(path, send) => {
                info!("got request for file {:?}", path);
                assert!(!self.pending_files.contains_key(&path));
                self.to_client
                    .send(GetBufferText {
                        file: path.to_string_lossy().into_owned(),
                    })
                    .unwrap();
                self.pending_files.insert(path, send);
            }

            // Other messages pass through to the main thread.  The channels we use are unbounded,
            // so if the main thread is busy (most importantly, if it's waiting on some file
            // contents), then the message will be queued.
            InputMessage(msg) => {
                self.to_main.send(msg).unwrap();
            }
        }
    }
}

pub fn run_worker(
    recv: Receiver<ToWorker>,
    to_client: SyncSender<ToClient>,
    to_main: Sender<ToServer>,
) {
    let mut state = WorkerState::new(to_client, to_main);
    state.run_loop(recv);
}
