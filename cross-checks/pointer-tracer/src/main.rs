#![feature(libc)]

#[macro_use]
extern crate clap;
extern crate libc;
extern crate nix;
extern crate spawn_ptrace;

use clap::{Arg, App, ArgMatches};
use nix::sys::ptrace;
use nix::sys::signal::Signal;
use nix::sys::wait::{waitpid, WaitStatus};
use nix::unistd;
use spawn_ptrace::CommandPtraceSpawn;
use std::io;
use std::mem;
use std::process;
use std::ptr;
use std::slice;

/// Marker pattern that identifies C2Rust invalid pointer checks
const C2RUST_MARKER: &'static [u8] = b"C2RUST_INVPTR";

fn get_process_regs(pid: unistd::Pid) -> libc::user_regs_struct {
    unsafe {
        let regs: libc::user_regs_struct = mem::uninitialized();
        #[allow(deprecated)]
        ptrace::ptrace(ptrace::Request::PTRACE_GETREGS,
                       pid, ptr::null_mut(),
                       mem::transmute(&regs))
            .expect("Error running ptrace()");
        regs
    }
}

fn set_process_regs(pid: unistd::Pid, regs: libc::user_regs_struct) {
    unsafe {
        #[allow(deprecated)]
        ptrace::ptrace(ptrace::Request::PTRACE_SETREGS,
                       pid, ptr::null_mut(),
                       mem::transmute(&regs))
            .expect("Error running ptrace()");
    }
}

/// Look for the marker at `pc` and return the offset to advance
/// the program counter by, if the marker is found
fn get_c2rust_pc_delta(pid: unistd::Pid, pc: usize) -> Option<isize> {
    let total_size = C2RUST_MARKER.len() + 3;
    let mut buf: Vec<libc::c_long> = vec![];
    let mut addr = pc - total_size;
    while addr < pc {
        let data = unsafe {
            #[allow(deprecated)]
            ptrace::ptrace(ptrace::Request::PTRACE_PEEKTEXT,
                           pid, addr as *mut nix::libc::c_void,
                           ptr::null_mut())
                .expect("Error running ptrace()")
        };
        buf.push(data);
        addr += mem::size_of::<libc::c_long>();
    }
    let buf_ptr = buf.as_ptr() as *const u8;
    let buf_marker = unsafe {
        slice::from_raw_parts(buf_ptr.offset(2), C2RUST_MARKER.len())
    };
    if buf_marker == C2RUST_MARKER {
        let delta = unsafe { *(buf_ptr as *const i16) };
        Some(delta as isize)
    } else {
        None
    }
}

fn run_command(matches: ArgMatches) -> Result<(), io::Error> {
    let cmd = process::Command::new(matches.value_of("cmd").unwrap())
        .args(matches.values_of("args").into_iter().flatten())
        .spawn_ptrace()?;
    let pid = cmd.id() as i32;
    let unistd_pid = unistd::Pid::from_raw(pid);
    ptrace::cont(unistd_pid, None)
        .expect("Error running ptrace()");

    // Main loop: loop until program exits or we get a signal
    'main_loop:
    loop {
        let res = waitpid(unistd_pid, None)
            .expect("Error waiting for command to end");
        match res {
            WaitStatus::Exited(_, _code) => {
                // Got an exit code
                // TODO: return it somehow???
                break 'main_loop;
            },
            WaitStatus::Signaled(_, sig, core_dumped) => {
                let core_dump_msg = if core_dumped {
                    ", core dumped"
                } else {
                    ""
                };
                eprintln!("Process {} terminated with signal {:?}{}",
                          pid, sig, core_dump_msg);
                break 'main_loop;
            }
            WaitStatus::Stopped(_, sig) => {
                if sig == Signal::SIGSEGV {
                    let mut regs = get_process_regs(unistd_pid);
                    let pc_delta = get_c2rust_pc_delta(unistd_pid,
                                                       regs.rip as usize);
                    if let Some(pc_delta) = pc_delta {
                        // Found the marker, advance the PC and continue execution
                        regs.rip += pc_delta as libc::c_ulong;
                        set_process_regs(unistd_pid, regs);
                        ptrace::cont(unistd_pid, None)
                            .expect("Error running ptrace()");
                        continue 'main_loop;
                    }
                }
                ptrace::cont(unistd_pid, Some(sig))
                    .expect("Error running ptrace()");
            },
            ws @ _ => panic!("Unknown wait status: {:?}", ws)
        }
    }
    Ok(())
}

fn main() -> Result<(), io::Error> {
    let matches = App::new("C2Rust cross-check pointer access tracer")
        .version(crate_version!())
        .author(crate_authors!())
        .arg(Arg::with_name("pid")
             .help("Attach to existing process instead of starting a new one")
             .short("p")
             .long("pid")
             .takes_value(true)
             .value_name("PID")
             )
        .arg(Arg::with_name("cmd")
             .help("Command to run")
             .conflicts_with("pid")
             .required_unless("pid")
             )
        .arg(Arg::with_name("args")
             .help("Arguments for command")
             .conflicts_with("pid")
             .multiple(true)
             .last(true)
             )
        .get_matches();

    if let Some(_pid) = matches.value_of("pid").map(String::from) {
        unimplemented!();
    } else {
        run_command(matches)
    }
}
