//! The main thread for interactive mode.
//!
//! The main thread runs a loop receiving and processing client requests.
use log::info;
use rustc_ast::visit::{self, AssocCtxt, FnKind, Visitor};
use rustc_ast::*;
use rustc_interface::interface::{self, Config};
use rustc_span::source_map::Span;
use rustc_span::source_map::{FileLoader, RealFileLoader};
use rustc_span::symbol::Symbol;
use rustc_span::FileName;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::mpsc::{self, Receiver, SyncSender};
use std::sync::{Arc, Mutex};
use std::thread;

use crate::ast_builder::IntoSymbol;
use crate::ast_manip::{GetNodeId, GetSpan, Visit};
use crate::command::{self, RefactorState};
use crate::driver;
use crate::file_io::FileIO;
use crate::interact::worker::{self, ToWorker};
use crate::interact::WrapSender;
use crate::interact::{plain_backend, vim8_backend};
use crate::interact::{ToClient, ToServer};
use crate::pick_node;
use crate::RefactorCtxt;

use super::MarkInfo;

struct InteractState {
    to_client: SyncSender<ToClient>,
    buffers_available: Arc<Mutex<HashSet<PathBuf>>>,

    state: RefactorState,
}

impl InteractState {
    fn new(
        state: RefactorState,
        buffers_available: Arc<Mutex<HashSet<PathBuf>>>,
        _to_worker: SyncSender<ToWorker>,
        to_client: SyncSender<ToClient>,
    ) -> InteractState {
        InteractState {
            to_client,
            buffers_available,
            state,
        }
    }

    fn run_loop(&mut self, main_recv: Receiver<ToServer>) {
        for msg in main_recv.iter() {
            let result = panic::catch_unwind(AssertUnwindSafe(|| {
                self.handle_one(msg);
            }));

            if let Err(e) = result {
                let text = if let Some(s) = e.downcast_ref::<String>() {
                    s.clone()
                } else {
                    "An error occurred of unknown type".to_owned()
                };
                self.to_client.send(ToClient::Error { text }).unwrap();
            }
        }
    }

    fn run_compiler<F, R>(&mut self, phase: driver::Phase, func: F) -> interface::Result<R>
    where
        F: FnOnce(&Crate, &RefactorCtxt) -> R,
    {
        self.state
            .transform_crate(phase, |st, cx| func(&st.krate(), cx))
    }

    fn handle_one(&mut self, msg: ToServer) {
        use super::ToClient::*;
        use super::ToServer::*;

        match msg {
            AddMark {
                file,
                line,
                col,
                kind,
                label,
            } => {
                let kind = pick_node::NodeKind::from_str(&kind).unwrap();
                let label = label.into_symbol();

                let (id, mark_info) = self
                    .run_compiler(driver::Phase::Phase2, |krate, cx| {
                        let info = pick_node::pick_node_at_loc(
                            &krate,
                            cx.session(),
                            kind,
                            &file,
                            line,
                            col,
                        )
                        .unwrap_or_else(|| {
                            panic!("no {:?} node at {}:{}:{}", kind, file, line, col)
                        });

                        let lo = cx.session().source_map().lookup_char_pos(info.span.lo());
                        let hi = cx.session().source_map().lookup_char_pos(info.span.hi());
                        let file = filename_to_str(&lo.file.name);
                        (
                            info.id,
                            MarkInfo {
                                id: info.id.as_usize(),
                                file,
                                start_line: lo.line as u32,
                                start_col: lo.col.0 as u32,
                                end_line: hi.line as u32,
                                end_col: hi.col.0 as u32,
                                labels: vec![(&label.as_str() as &str).to_owned()],
                            },
                        )
                    })
                    .expect("Failed to run compiler");

                self.state.marks_mut().insert((id, label));
                self.to_client.send(Mark { info: mark_info }).unwrap();
            }

            RemoveMark { id } => {
                self.state
                    .marks_mut()
                    .retain(|&(mark_id, _)| mark_id.as_usize() != id);
            }

            GetMarkInfo { id } => {
                let id = NodeId::from_usize(id);

                let mut labels = Vec::new();
                for &(mark_id, label) in &*self.state.marks() {
                    if mark_id == id {
                        labels.push((&label.as_str() as &str).to_owned());
                    }
                }
                labels.sort();

                let msg = self
                    .run_compiler(driver::Phase::Phase2, |_krate, cx| {
                        let hir_id = cx.hir_map().node_to_hir_id(id);
                        let span = cx.hir_map().span(hir_id);
                        let lo = cx.session().source_map().lookup_char_pos(span.lo());
                        let hi = cx.session().source_map().lookup_char_pos(span.hi());
                        let file = filename_to_str(&lo.file.name);
                        let info = MarkInfo {
                            id: id.as_usize(),
                            file,
                            start_line: lo.line as u32,
                            start_col: lo.col.0 as u32,
                            end_line: hi.line as u32,
                            end_col: hi.col.0 as u32,
                            labels,
                        };
                        Mark { info }
                    })
                    .expect("Failed to run compiler");
                self.to_client.send(msg).unwrap();
            }

            GetMarkList => {
                let msg = self
                    .state
                    .transform_crate(driver::Phase::Phase2, |st, cx| {
                        let infos = collect_mark_infos(&st.marks(), &st.krate(), &cx);
                        MarkList { infos }
                    })
                    .expect("Failed to run compiler");
                self.to_client.send(msg).unwrap();
            }

            SetBuffersAvailable { files } => {
                let mut buffers = self.buffers_available.lock().unwrap();
                *buffers = files
                    .into_iter()
                    .filter_map(|x| fs::canonicalize(&x).ok())
                    .collect();
            }

            RunCommand { name, args } => {
                info!("running command {} with args {:?}", name, args);
                self.state.load_crate();
                match self.state.run(&name, &args) {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("{e}");
                        panic!("Invalid command.");
                    }
                }
                self.state.save_crate();
            }

            // Other messages are handled by the worker thread
            BufferText { .. } => unreachable!(),
        }
    }
}

fn filename_to_str(filename: &FileName) -> String {
    match filename {
        &FileName::Real(ref rfn) => rfn
            .to_string_lossy(rustc_span::FileNameDisplayPreference::Local)
            .into_owned(),
        // TODO: FileName::Macros is gone, do we need an alternative?
        other => panic!("Need to implement name conversion for {:?}", other),
    }
}

fn collect_mark_infos(
    marks: &HashSet<(NodeId, Symbol)>,
    krate: &Crate,
    cx: &RefactorCtxt,
) -> Vec<MarkInfo> {
    let ids = marks.iter().map(|&(id, _)| id).collect();
    let span_map = collect_spans(krate, ids);

    let mut infos = HashMap::with_capacity(marks.len());
    for &(id, label) in marks {
        let info = infos.entry(id).or_insert_with(|| {
            let span = span_map[&id];
            let lo = cx.session().source_map().lookup_char_pos(span.lo());
            let hi = cx.session().source_map().lookup_char_pos(span.hi());
            let file = filename_to_str(&lo.file.name);

            MarkInfo {
                id: id.as_usize(),
                file,
                start_line: lo.line as u32,
                start_col: lo.col.0 as u32,
                end_line: hi.line as u32,
                end_col: hi.col.0 as u32,
                labels: vec![],
            }
        });
        info.labels.push((&label.as_str() as &str).to_owned());
    }

    let mut infos_vec = Vec::with_capacity(infos.len());
    for (_, mut info) in infos {
        info.labels.sort();
        infos_vec.push(info);
    }
    infos_vec.sort_by_key(|i| i.id);
    infos_vec
}

pub fn interact_command(args: &[String], config: Config, registry: command::Registry) {
    let (to_main, main_recv) = mpsc::channel();
    let (to_worker, worker_recv) = mpsc::sync_channel(1);

    let backend_to_worker = WrapSender::new(to_worker.clone(), ToWorker::InputMessage);
    let to_client = if !args.is_empty() && &args[0] == "vim8" {
        vim8_backend::init(backend_to_worker)
    } else {
        plain_backend::init(backend_to_worker)
    };

    let to_client_ = to_client.clone();
    thread::spawn(move || {
        worker::run_worker(worker_recv, to_client_, to_main);
    });

    let buffers_available = Arc::new(Mutex::new(HashSet::new()));

    let file_io = Arc::new(InteractiveFileIO {
        buffers_available: buffers_available.clone(),
        to_worker: to_worker.clone(),
        to_client: to_client.clone(),
    });

    driver::run_refactoring(config, registry, file_io, HashSet::new(), |state| {
        InteractState::new(state, buffers_available, to_worker, to_client).run_loop(main_recv);
    });
}

#[derive(Clone)]
struct InteractiveFileIO {
    buffers_available: Arc<Mutex<HashSet<PathBuf>>>,
    to_worker: SyncSender<ToWorker>,
    to_client: SyncSender<ToClient>,
}

impl FileIO for InteractiveFileIO {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        let canon = fs::canonicalize(path)?;

        let available = { self.buffers_available.lock().unwrap().contains(&canon) };

        if available {
            let (send, recv) = mpsc::sync_channel(1);
            self.to_worker
                .send(ToWorker::NeedFile(canon, send))
                .unwrap();
            Ok(recv.recv().unwrap())
        } else {
            RealFileLoader.read_file(&canon)
        }
    }

    fn write_file(&self, path: &Path, s: &str) -> io::Result<()> {
        let path = fs::canonicalize(path)?;
        self.to_client
            .send(ToClient::NewBufferText {
                file: path.to_str().unwrap().to_owned(),
                content: s.to_owned(),
            })
            .unwrap();
        Ok(())
    }
}

struct CollectSpanVisitor {
    ids: HashSet<NodeId>,
    spans: HashMap<NodeId, Span>,
}

impl CollectSpanVisitor {
    fn record<T: GetNodeId + GetSpan>(&mut self, x: &T) {
        if self.ids.contains(&x.get_node_id()) {
            self.spans.insert(x.get_node_id(), x.get_span());
        }
    }
}

impl<'ast> Visitor<'ast> for CollectSpanVisitor {
    fn visit_item(&mut self, x: &'ast Item) {
        self.record(x);
        visit::walk_item(self, x)
    }

    fn visit_assoc_item(&mut self, x: &'ast AssocItem, ctxt: AssocCtxt) {
        self.record(x);
        visit::walk_assoc_item(self, x, ctxt)
    }

    fn visit_foreign_item(&mut self, x: &'ast ForeignItem) {
        self.record(x);
        visit::walk_foreign_item(self, x)
    }

    fn visit_stmt(&mut self, x: &'ast Stmt) {
        self.record(x);
        visit::walk_stmt(self, x)
    }

    fn visit_expr(&mut self, x: &'ast Expr) {
        self.record(x);
        visit::walk_expr(self, x)
    }

    fn visit_pat(&mut self, x: &'ast Pat) {
        self.record(x);
        visit::walk_pat(self, x)
    }

    fn visit_ty(&mut self, x: &'ast Ty) {
        self.record(x);
        visit::walk_ty(self, x)
    }

    fn visit_fn(&mut self, kind: FnKind<'ast>, span: Span, _id: NodeId) {
        for arg in &kind.decl().inputs {
            if self.ids.contains(&arg.id) {
                self.spans.insert(arg.id, arg.pat.span.to(arg.ty.span));
            }
        }
        visit::walk_fn(self, kind, span);
    }
}

fn collect_spans<T: Visit>(target: &T, ids: HashSet<NodeId>) -> HashMap<NodeId, Span> {
    let mut v = CollectSpanVisitor {
        ids,
        spans: HashMap::new(),
    };
    target.visit(&mut v);
    v.spans
}
