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


enum ToMainThread {
    InputMessage(ToServer),
    NeedFile(PathBuf, Sender<String>),
}

struct InteractState {
    rustc_args: Vec<String>,
    to_main: Sender<ToMainThread>,
    to_client: Sender<ToClient>,

    registry: command::Registry,
    current_marks: HashSet<(NodeId, Symbol)>,

    buffers_available: HashSet<PathBuf>,
    pending_files: HashMap<PathBuf, Sender<String>>,
}

impl InteractState {
    fn new(rustc_args: Vec<String>,
           registry: command::Registry,
           to_main: Sender<ToMainThread>,
           to_client: Sender<ToClient>) -> InteractState {
        InteractState {
            rustc_args: rustc_args,
            to_main: to_main,
            to_client: to_client,

            registry: registry,
            current_marks: HashSet::new(),

            buffers_available: HashSet::new(),
            pending_files: HashMap::new(),
        }
    }

    fn run_loop(&mut self,
                main_recv: Receiver<ToMainThread>) {
        for msg in main_recv.iter() {
            let result = panic::catch_unwind(AssertUnwindSafe(|| {
                self.handle_one(msg);
            }));

            if let Err(e) = result {
                let text =
                    if let Some(s) = e.downcast_ref::<String>() { s.clone() }
                    else {
                        "An error occurred of unknown type".to_owned()
                    };
                self.to_client.send(ToClient::Error { text }).unwrap();
            }
        }
    }

    fn make_file_loader(&self) -> Box<FileLoader> {
        Box::new(InteractiveFileLoader {
            buffers_available: self.buffers_available.clone(),
            to_main: self.to_main.clone(),
            real: RealFileLoader,
        })
    }

    fn run_compiler<F, R>(&self, phase: driver::Phase, func: F) -> R
            where F: FnOnce(Crate, driver::Ctxt) -> R {
        let file_loader = self.make_file_loader();
        driver::run_compiler(&self.rustc_args, Some(file_loader), phase, func)
    }

    fn handle_one(&mut self, msg: ToMainThread) {
        use self::ToMainThread::*;
        use super::ToServer::*;
        use super::ToClient::*;

        match msg {
            InputMessage(AddMark { file, line, col, kind, label }) => {
                let kind = pick_node::NodeKind::from_str(&kind).unwrap();
                let label = label.into_symbol();

                let (id, msg) = self.run_compiler(driver::Phase::Phase2, |krate, cx| {
                    let info = pick_node::pick_node_at_loc(
                            &krate, &cx, kind, &file, line, col)
                        .unwrap_or_else(
                            || panic!("no {:?} node at {}:{}:{}", kind, file, line, col));

                    let lo = cx.session().codemap().lookup_char_pos(info.span.lo);
                    let hi = cx.session().codemap().lookup_char_pos(info.span.hi);
                    (info.id,
                     MarkInfo {
                         id: info.id.as_usize(),
                         file: lo.file.name.clone(),
                         start_line: lo.line as u32,
                         start_col: lo.col.0 as u32,
                         end_line: hi.line as u32,
                         end_col: hi.col.0 as u32,
                         labels: vec![(&label.as_str() as &str).to_owned()],
                     })
                });

                self.current_marks.insert((id, label));
                self.to_client.send(msg).unwrap();
            },

            InputMessage(RemoveMark { id }) => {
                self.current_marks.retain(|&(mark_id, _)| mark_id.as_usize() != id);
            },

            InputMessage(GetMarkInfo { id }) => {
                let id = NodeId::new(id);

                let mut labels = Vec::new();
                for &(mark_id, label) in &self.current_marks {
                    if mark_id == id {
                        labels.push((&label.as_str() as &str).to_owned());
                    }
                }
                labels.sort();

                let msg = self.run_compiler(driver::Phase::Phase2, |krate, cx| {
                    let span = cx.hir_map().span(id);
                    let lo = cx.session().codemap().lookup_char_pos(span.lo);
                    let hi = cx.session().codemap().lookup_char_pos(span.hi);
                    MarkInfo {
                        id: id.as_usize(),
                        file: lo.file.name.clone(),
                        start_line: lo.line as u32,
                        start_col: lo.col.0 as u32,
                        end_line: hi.line as u32,
                        end_col: hi.col.0 as u32,
                        labels: labels,
                    }
                });
                self.to_client.send(msg).unwrap();
            },

            InputMessage(GetNodeList) => {
                let mut nodes = Vec::with_capacity(self.current_marks.len());
                for &(id, _) in &self.current_marks {
                    nodes.push(id.as_usize());
                }
                nodes.sort();
                self.to_client.send(NodeList { nodes });
            },

            InputMessage(SetBuffersAvailable { files }) => {
                self.buffers_available = files.into_iter()
                    .map(|x| fs::canonicalize(&x).unwrap())
                    .collect();
            },

            InputMessage(BufferText { file, content }) => {
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

            InputMessage(RunCommand { name, args }) => {
                info!("getting command {} with args {:?}", name, args);
                let mut cmd = self.registry.get_command(&name, &args);
                let file_loader = self.make_file_loader();
                let phase = cmd.min_phase();
                info!("starting driver");
                driver::run_compiler(&self.rustc_args.clone(),
                                     Some(file_loader),
                                     phase,
                                     |krate, cx| {
                    let krate = span_fix::fix_spans(cx.session(), krate);

                    let mut cmd_state = CommandState::new(krate.clone(),
                                                          self.current_marks.clone());
                    info!("running command...");
                    cmd.run(&mut cmd_state, &cx);
                    info!("changes: {}, {}",
                          cmd_state.krate_changed(),
                          cmd_state.marks_changed());

                    if cmd_state.krate_changed() {
                        let rws = rewrite::rewrite(cx.session(), &krate, &cmd_state.krate());
                        file_rewrite::rewrite_files_with(cx.session().codemap(), &rws, |fm, s| {
                            info!("got new text for {:?}", fm.name);
                            if fm.name.starts_with("<") {
                                return;
                            }

                            self.to_client.send(NewBufferText {
                                file: fm.name.clone(),
                                content: s.to_owned(),
                            }).unwrap();
                        });
                    }

                    if cmd_state.marks_changed() {
                        self.current_marks = cmd_state.marks().clone();
                    }
                });
            },

            NeedFile(path, send) => {
                assert!(!self.pending_files.contains_key(&path));
                self.to_client.send(GetBufferText {
                    file: path.to_string_lossy().into_owned(),
                }).unwrap();
                self.pending_files.insert(path, send);
            },
        }
    }
}

pub fn interact_command(args: &[String],
                        rustc_args: Vec<String>,
                        registry: command::Registry) {
    let (main_send, main_recv) = mpsc::channel();

    let server_send = WrapSender::new(main_send.clone(), ToMainThread::InputMessage);
    let to_client =
        if args.len() > 0 && &args[0] == "vim8" { vim8_backend::init(server_send) }
        else { plain_backend::init(server_send) };

    InteractState::new(rustc_args, registry, main_send, to_client)
        .run_loop(main_recv);
}


struct InteractiveFileLoader {
    buffers_available: HashSet<PathBuf>,
    to_main: Sender<ToMainThread>,
    real: RealFileLoader,
}

impl FileLoader for InteractiveFileLoader {
    fn file_exists(&self, path: &Path) -> bool {
        self.real.file_exists(path)
    }

    fn abs_path(&self, path: &Path) -> Option<PathBuf> {
        self.real.abs_path(path)
    }

    fn read_file(&self, path: &Path) -> io::Result<String> {
        let canon = fs::canonicalize(path)?;

        if self.buffers_available.contains(&canon) {
            let (send, recv) = mpsc::channel();
            self.to_main.send(ToMainThread::NeedFile(canon, send)).unwrap();
            Ok(recv.recv().unwrap())
        } else {
            self.real.read_file(&canon)
        }
    }
}
