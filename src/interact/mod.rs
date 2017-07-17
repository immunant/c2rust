use std::marker::PhantomData;
use std::sync::mpsc::{Sender, SendError};

mod plain_backend;
mod vim8_backend;
mod worker;
mod main_thread;

pub use self::main_thread::interact_command;


#[derive(Clone, Debug)]
pub enum ToServer {
    AddMark {
        file: String,
        line: u32,
        col: u32,
        kind: String,
        label: String,
    },

    RemoveMark {
        id: usize,
    },

    GetMarkInfo {
        id: usize,
    },

    GetNodeList,


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


    RunCommand {
        name: String,
        args: Vec<String>,
    },
}

#[derive(Clone, Debug)]
pub enum ToClient {
    /// Details about an existing mark.
    MarkInfo {
        id: usize,
        file: String,
        start_line: u32,
        start_col: u32,
        end_line: u32,
        end_col: u32,
        labels: Vec<String>,
    },

    /// List the IDs of all marked nodes.
    NodeList {
        nodes: Vec<usize>,
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


#[derive(Clone, Debug)]
pub struct WrapSender<T, U, F> where F: Fn(T) -> U {
    inner: Sender<U>,
    convert: F,
    _marker: PhantomData<T>,
}

impl<T, U, F> WrapSender<T, U, F> where F: Fn(T) -> U {
    pub fn new(inner: Sender<U>, convert: F) -> WrapSender<T, U, F> {
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
