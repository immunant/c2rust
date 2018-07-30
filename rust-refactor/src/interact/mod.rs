//! Interactive mode, for running `idiomize` as a backend for editor plugins.
use std::marker::PhantomData;
use std::sync::mpsc::{SyncSender, SendError};

mod plain_backend;
mod vim8_backend;
mod worker;
mod main_thread;

pub use self::main_thread::interact_command;


#[derive(Clone, Debug)]
pub enum ToServer {
    /// Add a mark with label `label` to a node of the indicated `kind` at `file`, `line`, `col`.
    AddMark {
        file: String,
        line: u32,
        col: u32,
        kind: String,
        label: String,
    },

    /// Remove all marks from node `id`.
    RemoveMark {
        id: usize,
    },

    /// Get details about the marks on node `id`.
    GetMarkInfo {
        id: usize,
    },

    /// Get a list of all marks.
    GetMarkList,


    /// Provide the server with a list of available buffers.  If the compiler would load one of the
    /// named files, the server will request its contents from the client, instead of reading the
    /// contents on disk.
    SetBuffersAvailable {
        files: Vec<String>,
    },

    /// Provide the server with the contents of a buffer.
    BufferText {
        file: String,
        content: String,
    },


    /// Run a refactoring command.
    RunCommand {
        name: String,
        args: Vec<String>,
    },
}

#[derive(Clone, Debug)]
pub struct MarkInfo {
    id: usize,
    file: String,
    start_line: u32,
    start_col: u32,
    end_line: u32,
    end_col: u32,
    labels: Vec<String>,
}

#[derive(Clone, Debug)]
pub enum ToClient {
    /// Details about an existing mark.
    Mark {
        info: MarkInfo,
    },

    /// List the IDs of all marked nodes.
    MarkList {
        infos: Vec<MarkInfo>,
    },


    /// Request buffer text from the client.
    GetBufferText {
        file: String,
    },


    /// Rewritten buffer text
    NewBufferText {
        file: String,
        content: String,
    },

    Error {
        text: String,
    },
}


/// Like `std::sync::mpsc::Sender`, but transforms sent data with a function before sending it to
/// the receiving thread.
#[derive(Clone, Debug)]
pub struct WrapSender<T, U, F> where F: Fn(T) -> U {
    inner: SyncSender<U>,
    convert: F,
    _marker: PhantomData<T>,
}

impl<T, U, F> WrapSender<T, U, F> where F: Fn(T) -> U {
    pub fn new(inner: SyncSender<U>, convert: F) -> WrapSender<T, U, F> {
        WrapSender {
            inner: inner,
            convert: convert,
            _marker: PhantomData,
        }
    }

    pub fn send(&self, t: T) -> Result<(), SendError<U>> {
        let u = (self.convert)(t);
        self.inner.send(u)
    }
}
