#!/usr/bin/env -S uv run

# -*- coding: utf-8 -*-

from plumbum import local
from typing import Tuple
from common import (
    Colors,
    Config,
    Command,
    die,
    pb,
    setup_logging,
    transpile
)

import argparse
import errno
import os
import re

mv = local["mv"]

desc = 'transpile files in compiler_commands.json.'
parser = argparse.ArgumentParser(description="Translates tmux into the repo/rust/src directory")
parser.add_argument('-f', '--filter',
                    default="",
                    help='Filters translated files')
config = Config()
config.add_args(parser)

C2RUST_DIR = config.ROOT_DIR
TMUX_REPO = os.path.join(C2RUST_DIR, "examples/tmux/repo")
TMUX_RS = os.path.join(TMUX_REPO, "tmux.rs")
TMUX_COMPAT_DIR = os.path.join(TMUX_REPO, "compat")
COMPILE_COMMANDS = os.path.join(TMUX_REPO, "compile_commands.json")
RUST_ROOT_DIR = os.path.join(TMUX_REPO, "rust")
RUST_SRC_DIR = os.path.join(RUST_ROOT_DIR, "src")
MAIN_RS = os.path.join(RUST_SRC_DIR, "main.rs")
RUST_COMPAT_DIR = os.path.join(RUST_SRC_DIR, "compat")
FILES_NEEDING_TRAILING_UNDERSCORE = [
    "client.rs",
    "cmd.rs",
    "cmd_list.rs",
    "environ.rs",
    "grid.rs",
    "hooks.rs",
    "job.rs",
    "key_bindings.rs",
    "options.rs",
    "proc.rs",
    "screen.rs",
    "session.rs",
    "tty.rs",
    "tty_term.rs",
    "window.rs",
]
# TODO(kkysen) shouldn't need `extern crate`
MAIN_MODS = """\
#![feature(label_break_value)]
#![allow(unused_imports)]
extern crate libc;

pub mod alerts;
pub mod arguments;
pub mod attributes;
pub mod cfg;
pub mod client_;
pub mod cmd_;
pub mod cmd_attach_session;
pub mod cmd_bind_key;
pub mod cmd_break_pane;
pub mod cmd_capture_pane;
pub mod cmd_choose_tree;
pub mod cmd_command_prompt;
pub mod cmd_confirm_before;
pub mod cmd_copy_mode;
pub mod cmd_detach_client;
pub mod cmd_display_message;
pub mod cmd_display_panes;
pub mod cmd_find_window;
pub mod cmd_find;
pub mod cmd_if_shell;
pub mod cmd_join_pane;
pub mod cmd_kill_pane;
pub mod cmd_kill_server;
pub mod cmd_kill_session;
pub mod cmd_kill_window;
pub mod cmd_list_;
pub mod cmd_list_buffers;
pub mod cmd_list_clients;
pub mod cmd_list_keys;
pub mod cmd_list_panes;
pub mod cmd_list_sessions;
pub mod cmd_list_windows;
pub mod cmd_load_buffer;
pub mod cmd_lock_server;
pub mod cmd_move_window;
pub mod cmd_new_session;
pub mod cmd_new_window;
pub mod cmd_paste_buffer;
pub mod cmd_pipe_pane;
pub mod cmd_queue;
pub mod cmd_refresh_client;
pub mod cmd_rename_session;
pub mod cmd_rename_window;
pub mod cmd_resize_pane;
pub mod cmd_respawn_pane;
pub mod cmd_respawn_window;
pub mod cmd_rotate_window;
pub mod cmd_run_shell;
pub mod cmd_save_buffer;
pub mod cmd_select_layout;
pub mod cmd_select_pane;
pub mod cmd_select_window;
pub mod cmd_send_keys;
pub mod cmd_set_buffer;
pub mod cmd_set_environment;
pub mod cmd_set_hook;
pub mod cmd_set_option;
pub mod cmd_show_environment;
pub mod cmd_show_messages;
pub mod cmd_show_options;
pub mod cmd_source_file;
pub mod cmd_split_window;
pub mod cmd_string;
pub mod cmd_swap_pane;
pub mod cmd_swap_window;
pub mod cmd_switch_client;
pub mod cmd_unbind_key;
pub mod cmd_wait_for;
pub mod colour;
pub mod compat;
pub mod control_notify;
pub mod control;
pub mod environ_;
pub mod format;
pub mod grid_;
pub mod grid_view;
pub mod hooks_;
pub mod input;
pub mod input_keys;
pub mod job_;
pub mod key_bindings_;
pub mod key_string;
pub mod layout_custom;
pub mod layout_set;
pub mod layout;
pub mod log;
pub mod mode_tree;
pub mod names;
pub mod notify;
pub mod options_;
#[cfg(target_os = "linux")]
pub mod osdep_linux;
pub mod options_table;
pub mod paste;
pub mod proc_;
pub mod resize;
pub mod screen_;
pub mod screen_redraw;
pub mod screen_write;
pub mod server_client;
pub mod server_fn;
pub mod server;
pub mod session_;
pub mod status;
pub mod style;
pub mod tty_;
pub mod tty_acs;
pub mod tty_keys;
pub mod tty_term_;
pub mod utf8;
pub mod window_;
pub mod window_buffer;
pub mod window_client;
pub mod window_clock;
pub mod window_copy;
pub mod window_tree;
pub mod xmalloc;
pub mod xterm_keys;
"""
Retcode = int
StdErr = str
StdOut = str


def move(from_, to) -> Tuple[Retcode, StdOut, StdErr]:
    mv_args = [from_, to]

    return mv[mv_args].run()


def get_rename_cmd() -> Command:
    try:
        return pb.local['prename']
    except pb.CommandNotFound:
        try:
            return pb.local['perl-rename']
        except pb.CommandNotFound:
            die("prename and perl-rename not in path", errno.ENOENT)


def rename_(*args) -> Tuple[Retcode, StdOut, StdErr]:
    return get_rename_cmd()[args].run()


def add_mods(path: str):
    with open(path, "r+") as file:
        text = file.read()
        # TODO(kkysen) shouldn't need `extern crate`
        text = re.sub(r"extern crate libc;", MAIN_MODS, text, count=1)

        file.seek(0)
        file.write(text)
        file.truncate()


if __name__ == "__main__":
    setup_logging()
    args = parser.parse_args()

    # Add option to use the debug version of `c2rust`
    config.update_args(args)

    assert os.path.isfile(COMPILE_COMMANDS), "Could not find {}".format(COMPILE_COMMANDS)

    print(Colors.OKBLUE + "Transpiling..." + Colors.NO_COLOR)
    transpile(COMPILE_COMMANDS, emit_build_files=False,
              reorganize_definitions=True,
              extra_transpiler_args=["--reduce-type-annotations"])

    # Move and rename tmux.rs to main.rs
    move(TMUX_RS, MAIN_RS)

    plumbum_rs_glob = local.path(TMUX_REPO) // "*.rs"
    plumbum_compat_rs_glob = local.path(TMUX_COMPAT_DIR) // "*.rs"

    # Move source files to src directory
    move(plumbum_rs_glob, RUST_SRC_DIR)

    # Move compat files to src/compat directory
    retcode, _, stderr = move(plumbum_compat_rs_glob, RUST_COMPAT_DIR)

    assert retcode != 1, "Could not move translated rs files:\n{}".format(stderr)

    # Some tmux files have the same file names as structs, so we also have to append
    # an underscore to the filename so that rust doesn't get confused
    files_needing_trailing_underscore = [os.path.join(RUST_SRC_DIR, file) for file in FILES_NEEDING_TRAILING_UNDERSCORE]

    rename_("s/.rs/_.rs/g", "-f", *files_needing_trailing_underscore)

    # main.rs needs to know about modules so we add them here
    add_mods(MAIN_RS)
    print(Colors.OKGREEN + "Done!" + Colors.NO_COLOR)
