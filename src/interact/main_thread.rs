use std::collections::{HashMap, HashSet};
use std::fs;
use std::io;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::mpsc::{self, Sender, Receiver};
use std::thread;
use syntax::ast::*;
use syntax::codemap::{FileLoader, RealFileLoader};
use syntax::codemap::Span;
use syntax::symbol::Symbol;
use syntax::visit::{self, Visitor, FnKind};

use command::{self, CommandState};
use driver;
use file_rewrite;
use get_node_id::GetNodeId;
use get_span::GetSpan;
use interact::{ToServer, ToClient};
use interact::WrapSender;
use interact::{plain_backend, vim8_backend};
use interact::worker::{self, ToWorker};
use pick_node;
use rewrite;
use span_fix;
use util::IntoSymbol;
use visit::Visit;

use super::MarkInfo;


struct InteractState {
    rustc_args: Vec<String>,
    to_worker: Sender<ToWorker>,
    to_client: Sender<ToClient>,
    buffers_available: HashSet<PathBuf>,

    registry: command::Registry,
    current_marks: HashSet<(NodeId, Symbol)>,
}

impl InteractState {
    fn new(rustc_args: Vec<String>,
           registry: command::Registry,
           to_worker: Sender<ToWorker>,
           to_client: Sender<ToClient>) -> InteractState {
        InteractState {
            rustc_args: rustc_args,
            to_worker: to_worker,
            to_client: to_client,
            buffers_available: HashSet::new(),

            registry: registry,
            current_marks: HashSet::new(),
        }
    }

    fn run_loop(&mut self,
                main_recv: Receiver<ToServer>) {
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
            to_worker: self.to_worker.clone(),
            real: RealFileLoader,
        })
    }

    fn run_compiler<F, R>(&self, phase: driver::Phase, func: F) -> R
            where F: FnOnce(Crate, driver::Ctxt) -> R {
        let file_loader = self.make_file_loader();
        driver::run_compiler(&self.rustc_args, Some(file_loader), phase, func)
    }

    fn collect_mark_infos(&self, krate: &Crate, cx: &driver::Ctxt) -> Vec<MarkInfo> {
        let ids = self.current_marks.iter().map(|&(id, _)| id).collect();
        let span_map = collect_spans(krate, ids);

        let mut infos = HashMap::with_capacity(self.current_marks.len());
        for &(id, label) in &self.current_marks {
            let info = infos.entry(id).or_insert_with(|| {
                let span = span_map[&id];
                let lo = cx.session().codemap().lookup_char_pos(span.lo);
                let hi = cx.session().codemap().lookup_char_pos(span.hi);
                MarkInfo {
                    id: id.as_usize(),
                    file: lo.file.name.clone(),
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

    fn handle_one(&mut self, msg: ToServer) {
        use super::ToServer::*;
        use super::ToClient::*;

        match msg {
            AddMark { file, line, col, kind, label } => {
                let kind = pick_node::NodeKind::from_str(&kind).unwrap();
                let label = label.into_symbol();

                let (id, mark_info) = self.run_compiler(driver::Phase::Phase2, |krate, cx| {
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
                self.to_client.send(Mark { info: mark_info }).unwrap();
            },

            RemoveMark { id } => {
                self.current_marks.retain(|&(mark_id, _)| mark_id.as_usize() != id);
            },

            GetMarkInfo { id } => {
                let id = NodeId::new(id);

                let mut labels = Vec::new();
                for &(mark_id, label) in &self.current_marks {
                    if mark_id == id {
                        labels.push((&label.as_str() as &str).to_owned());
                    }
                }
                labels.sort();

                let msg = self.run_compiler(driver::Phase::Phase2, |_krate, cx| {
                    let span = cx.hir_map().span(id);
                    let lo = cx.session().codemap().lookup_char_pos(span.lo);
                    let hi = cx.session().codemap().lookup_char_pos(span.hi);
                    let info = MarkInfo {
                        id: id.as_usize(),
                        file: lo.file.name.clone(),
                        start_line: lo.line as u32,
                        start_col: lo.col.0 as u32,
                        end_line: hi.line as u32,
                        end_col: hi.col.0 as u32,
                        labels: labels,
                    };
                    Mark { info: info }
                });
                self.to_client.send(msg).unwrap();
            },

            GetMarkList => {
                let msg = self.run_compiler(driver::Phase::Phase2, |krate, cx| {
                    let infos = self.collect_mark_infos(&krate, &cx);
                    MarkList { infos: infos }
                });
                self.to_client.send(msg).unwrap();
            },

            SetBuffersAvailable { files } => {
                self.buffers_available = files.into_iter()
                    .filter_map(|x| fs::canonicalize(&x).ok())
                    .collect();
            },

            RunCommand { name, args } => {
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

                        let infos = self.collect_mark_infos(&krate, &cx);
                        self.to_client.send(MarkList { infos: infos }).unwrap();
                    }
                });
            },

            // Other messages are handled by the worker thread
            BufferText { .. } => unreachable!(),
        }
    }
}

pub fn interact_command(args: &[String],
                        rustc_args: Vec<String>,
                        registry: command::Registry) {
    let (to_main, main_recv) = mpsc::channel();
    let (to_worker, worker_recv) = mpsc::channel();

    let backend_to_worker = WrapSender::new(to_worker.clone(), ToWorker::InputMessage);
    let to_client =
        if args.len() > 0 && &args[0] == "vim8" { vim8_backend::init(backend_to_worker) }
        else { plain_backend::init(backend_to_worker) };

    let to_client_ = to_client.clone();
    thread::spawn(move || {
        worker::run_worker(worker_recv, to_client_, to_main);
    });

    InteractState::new(rustc_args, registry, to_worker, to_client)
        .run_loop(main_recv);
}


struct InteractiveFileLoader {
    buffers_available: HashSet<PathBuf>,
    to_worker: Sender<ToWorker>,
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
            self.to_worker.send(ToWorker::NeedFile(canon, send)).unwrap();
            Ok(recv.recv().unwrap())
        } else {
            self.real.read_file(&canon)
        }
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

    fn visit_trait_item(&mut self, x: &'ast TraitItem) {
        self.record(x);
        visit::walk_trait_item(self, x)
    }

    fn visit_impl_item(&mut self, x: &'ast ImplItem) {
        self.record(x);
        visit::walk_impl_item(self, x)
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

    fn visit_fn(&mut self, kind: FnKind<'ast>, fd: &'ast FnDecl, span: Span, id: NodeId) {
        for arg in &fd.inputs {
            if self.ids.contains(&arg.id) {
                self.spans.insert(arg.id, arg.pat.span.to(arg.ty.span));
            }
        }
        visit::walk_fn(self, kind, fd, span);
    }
}

fn collect_spans<T: Visit>(target: &T, ids: HashSet<NodeId>) -> HashMap<NodeId, Span> {
    let mut v = CollectSpanVisitor {
        ids: ids,
        spans: HashMap::new(),
    };
    target.visit(&mut v);
    v.spans
}
