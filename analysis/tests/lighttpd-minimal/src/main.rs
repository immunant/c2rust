#![feature(rustc_private)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_mut)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![feature(extern_types)]

extern crate libc;

use libc::*;
use std::mem;

extern "C" {
    pub type pcre2_real_match_data_8;
    pub type lshpack_double_enc_head;
    pub type lshpack_enc_table_entry;
    // pub type fdevents;
    fn clock_gettime(__clock_id: clockid_t, __tp: *mut timespec) -> libc::c_int;
    fn buffer_copy_string_len(b: *mut buffer, s: *const libc::c_char, len: size_t);
    fn buffer_append_string_len(b: *mut buffer, s: *const libc::c_char, len: size_t);
    fn buffer_append_str2(
        b: *mut buffer,
        s1: *const libc::c_char,
        len1: size_t,
        s2: *const libc::c_char,
        len2: size_t,
    );
    fn buffer_eq_icase_slen(b: *const buffer, s: *const libc::c_char, slen: size_t) -> libc::c_int;
    fn hex2int(c: libc::c_uchar) -> libc::c_char;
    fn ck_assert_failed(
        filename: *const libc::c_char,
        line: libc::c_uint,
        msg: *const libc::c_char,
    ) -> !;
    fn chunkqueue_init(cq: *mut chunkqueue) -> *mut chunkqueue;
    fn chunkqueue_append_mem(cq: *mut chunkqueue, mem: *const libc::c_char, len: size_t);
    fn chunkqueue_append_chunkqueue(cq: *mut chunkqueue, src: *mut chunkqueue);
    fn chunkqueue_append_buffer_open(cq: *mut chunkqueue) -> *mut buffer;
    fn chunkqueue_append_buffer_commit(cq: *mut chunkqueue);
    fn chunkqueue_get_memory(cq: *mut chunkqueue, len: *mut size_t) -> *mut libc::c_char;
    fn chunkqueue_use_memory(cq: *mut chunkqueue, ckpt: *mut chunk, len: size_t);
    fn chunkqueue_mark_written(cq: *mut chunkqueue, len: off_t);
    fn chunkqueue_remove_finished_chunks(cq: *mut chunkqueue);
    fn chunkqueue_steal(dest: *mut chunkqueue, src: *mut chunkqueue, len: off_t);
    fn chunkqueue_steal_with_tempfiles(
        dest: *mut chunkqueue,
        src: *mut chunkqueue,
        len: off_t,
        errh: *mut log_error_st,
    ) -> libc::c_int;
    fn chunkqueue_compact_mem_offset(cq: *mut chunkqueue);
    fn chunkqueue_compact_mem(cq: *mut chunkqueue, clen: size_t);
    fn chunkqueue_free(cq: *mut chunkqueue);
    fn chunkqueue_reset(cq: *mut chunkqueue);
    fn http_status_append(b: *mut buffer, status: libc::c_int);
    fn http_request_headers_process(
        r: *mut request_st,
        hdrs: *mut libc::c_char,
        hoff: *const libc::c_ushort,
        scheme_port: libc::c_int,
    );
    fn setsockopt(
        __fd: libc::c_int,
        __level: libc::c_int,
        __optname: libc::c_int,
        __optval: *const libc::c_void,
        __optlen: socklen_t,
    ) -> libc::c_int;
    fn shutdown(__fd: libc::c_int, __how: libc::c_int) -> libc::c_int;
    static mut log_con_jqueue: *mut connection;
    fn strchr(_: *const libc::c_char, _: libc::c_int) -> *mut libc::c_char;
    fn strstr(_: *const libc::c_char, _: *const libc::c_char) -> *mut libc::c_char;
    static mut log_epoch_secs: unix_time64_t;
    static mut log_monotonic_secs: unix_time64_t;
    fn log_error(
        errh: *mut log_error_st,
        filename: *const libc::c_char,
        line: libc::c_uint,
        fmt: *const libc::c_char,
        _: ...
    );
    fn log_perror(
        errh: *mut log_error_st,
        filename: *const libc::c_char,
        line: libc::c_uint,
        fmt: *const libc::c_char,
        _: ...
    );
    fn log_error_multiline(
        errh: *mut log_error_st,
        filename: *const libc::c_char,
        line: libc::c_uint,
        multiline: *const libc::c_char,
        len: size_t,
        fmt: *const libc::c_char,
        _: ...
    );
    // fn fdevent_fdnode_event_del(ev: *mut fdevents, fdn: *mut fdnode);
    fn fdevent_fdnode_event_set(ev: *mut fdevents, fdn: *mut fdnode, events: libc::c_int);
    // fn fdevent_register(
    //     ev: *mut fdevents,
    //     fd: libc::c_int,
    //     handler: fdevent_handler,
    //     ctx: *mut libc::c_void,
    // ) -> *mut fdnode;
    // fn fdevent_unregister(ev: *mut fdevents, fd: libc::c_int);
    fn fdevent_socket_read_discard(
        fd: libc::c_int,
        buf: *mut libc::c_char,
        sz: size_t,
        family: libc::c_int,
        so_type: libc::c_int,
    ) -> ssize_t;
    fn fdevent_ioctl_fionread(
        fd: libc::c_int,
        fdfmt: libc::c_int,
        toread: *mut libc::c_int,
    ) -> libc::c_int;
    fn fdevent_is_tcp_half_closed(fd: libc::c_int) -> libc::c_int;
    fn h2_init_con(h2r: *mut request_st, con: *mut connection, http2_settings: *const buffer);
    fn h2_send_goaway(con: *mut connection, e: request_h2error_t);
    fn h2_want_read(con: *mut connection) -> libc::c_int;
    fn h2_parse_frames(con: *mut connection) -> libc::c_int;
    fn h2_send_100_continue(r: *mut request_st, con: *mut connection);
    fn h2_send_headers(r: *mut request_st, con: *mut connection);
    fn h2_send_cqdata(
        r: *mut request_st,
        con: *mut connection,
        cq: *mut chunkqueue,
        dlen: uint32_t,
    ) -> uint32_t;
    fn h2_send_end_stream(r: *mut request_st, con: *mut connection);
    fn h2_retire_stream(r: *mut request_st, con: *mut connection);
    fn h2_retire_con(h2r: *mut request_st, con: *mut connection);
    fn h2_check_con_upgrade_h2c(r: *mut request_st) -> libc::c_int;
    fn http_header_request_get(
        r: *const request_st,
        id: http_header_e,
        k: *const libc::c_char,
        klen: uint32_t,
    ) -> *mut buffer;
    fn http_header_request_unset(
        r: *mut request_st,
        id: http_header_e,
        k: *const libc::c_char,
        klen: uint32_t,
    );
    fn http_header_parse_hoff(
        n: *const libc::c_char,
        clen: uint32_t,
        hoff: *mut libc::c_ushort,
    ) -> uint32_t;
    fn request_init_data(r: *mut request_st, con: *mut connection, srv: *mut server);
    fn request_reset(r: *mut request_st);
    fn request_reset_ex(r: *mut request_st);
    fn request_free_data(r: *mut request_st);
    fn http_response_reqbody_read_error(r: *mut request_st, http_status: libc::c_int) -> handler_t;
    fn http_response_write_header(r: *mut request_st);
    fn http_response_handler(r: *mut request_st) -> handler_t;
    fn plugins_call_handle_request_done(r: *mut request_st) -> handler_t;
    fn plugins_call_handle_connection_accept(con: *mut connection) -> handler_t;
    fn plugins_call_handle_connection_shut_wr(con: *mut connection) -> handler_t;
    fn plugins_call_handle_connection_close(con: *mut connection) -> handler_t;
    fn config_cond_cache_reset(r: *mut request_st);
    fn sock_addr_cache_inet_ntop_copy_buffer(
        b: *mut buffer,
        saddr: *const sock_addr,
    ) -> libc::c_int;
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn close(__fd: libc::c_int) -> libc::c_int;
    fn read(__fd: libc::c_int, __buf: *mut libc::c_void, __nbytes: size_t) -> ssize_t;
    fn __errno_location() -> *mut libc::c_int;
}

pub type __int8_t = libc::c_schar;
pub type __uint8_t = libc::c_uchar;
pub type __uint16_t = libc::c_ushort;
pub type __int32_t = libc::c_int;
pub type __uint32_t = libc::c_uint;
pub type __uint64_t = libc::c_ulong;
pub type __dev_t = libc::c_ulong;
pub type __uid_t = libc::c_uint;
pub type __gid_t = libc::c_uint;
pub type __ino_t = libc::c_ulong;
pub type __mode_t = libc::c_uint;
pub type __nlink_t = libc::c_ulong;
pub type __off_t = libc::c_long;
pub type __off64_t = libc::c_long;
pub type __pid_t = libc::c_int;
pub type __time_t = libc::c_long;
pub type __clockid_t = libc::c_int;
pub type __blksize_t = libc::c_long;
pub type __blkcnt_t = libc::c_long;
pub type __ssize_t = libc::c_long;
pub type __syscall_slong_t = libc::c_long;
pub type __socklen_t = libc::c_uint;
pub type gid_t = __gid_t;
pub type uid_t = __uid_t;
pub type off_t = __off64_t;
pub type pid_t = __pid_t;
pub type ssize_t = __ssize_t;
pub type clockid_t = __clockid_t;
pub type time_t = __time_t;
pub type size_t = libc::c_ulong;
pub type int8_t = __int8_t;
pub type int32_t = __int32_t;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct timespec {
    pub tv_sec: __time_t,
    pub tv_nsec: __syscall_slong_t,
}
pub type uint8_t = __uint8_t;
pub type uint16_t = __uint16_t;
pub type uint32_t = __uint32_t;
pub type uint64_t = __uint64_t;
pub type uintptr_t = libc::c_ulong;
pub type unix_time64_t = time_t;
pub type unix_timespec64_t = timespec;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct server {
    pub plugin_slots: *mut libc::c_void,
    pub config_context: *mut array,
    pub config_captures: libc::c_int,
    pub ev: *mut fdevents,
    pub network_backend_write: Option<
        unsafe extern "C" fn(libc::c_int, *mut chunkqueue, off_t, *mut log_error_st) -> libc::c_int,
    >,
    pub request_env: Option<unsafe extern "C" fn(*mut request_st) -> handler_t>,
    pub tmp_buf: *mut buffer,
    pub con_opened: libc::c_int,
    pub con_read: libc::c_int,
    pub con_written: libc::c_int,
    pub con_closed: libc::c_int,
    pub max_fds: libc::c_int,
    pub max_fds_lowat: libc::c_int,
    pub max_fds_hiwat: libc::c_int,
    pub cur_fds: libc::c_int,
    pub sockets_disabled: libc::c_int,
    pub lim_conns: uint32_t,
    pub conns: *mut connection,
    pub conns_pool: *mut connection,
    pub errh: *mut log_error_st,
    pub loadts: unix_time64_t,
    pub loadavg: [libc::c_double; 3],
    pub srvconf: server_config,
    pub config_data_base: *mut libc::c_void,
    pub srv_sockets: server_socket_array,
    pub srv_sockets_inherited: server_socket_array,
    pub plugins: C2RustUnnamed,
    pub startup_ts: unix_time64_t,
    pub graceful_expire_ts: unix_time64_t,
    pub uid: uid_t,
    pub gid: gid_t,
    pub pid: pid_t,
    pub stdin_fd: libc::c_int,
    pub argv: *mut *mut libc::c_char,
    pub match_data: *mut libc::c_void,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed {
    pub ptr: *mut libc::c_void,
    pub used: uint32_t,
    pub size: uint32_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct server_socket_array {
    pub ptr: *mut *mut server_socket,
    pub size: uint32_t,
    pub used: uint32_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct server_socket {
    pub addr: sock_addr,
    pub fd: libc::c_int,
    pub is_ssl: uint8_t,
    pub srv_token_colon: uint8_t,
    pub sidx: libc::c_ushort,
    pub fdn: *mut fdnode,
    pub srv: *mut server,
    pub srv_token: *mut buffer,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct buffer {
    pub ptr: *mut libc::c_char,
    pub used: uint32_t,
    pub size: uint32_t,
}
pub type fdnode = fdnode_st;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct fdnode_st {
    pub handler: fdevent_handler,
    pub ctx: *mut libc::c_void,
    pub fd: libc::c_int,
    pub events: libc::c_int,
    pub fde_ndx: libc::c_int,
}
pub type fdevent_handler =
    Option<unsafe extern "C" fn(*mut libc::c_void, libc::c_int) -> handler_t>;
pub type handler_t = libc::c_uint;
pub const HANDLER_ERROR: handler_t = 4;
pub const HANDLER_WAIT_FOR_EVENT: handler_t = 3;
pub const HANDLER_COMEBACK: handler_t = 2;
pub const HANDLER_FINISHED: handler_t = 1;
pub const HANDLER_GO_ON: handler_t = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub union sock_addr {
    pub ipv6: sockaddr_in6,
    pub ipv4: sockaddr_in,
    pub un: sockaddr_un,
    pub plain: sockaddr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct sockaddr {
    pub sa_family: sa_family_t,
    pub sa_data: [libc::c_char; 14],
}
pub type sa_family_t = libc::c_ushort;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct sockaddr_un {
    pub sun_family: sa_family_t,
    pub sun_path: [libc::c_char; 108],
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct sockaddr_in {
    pub sin_family: sa_family_t,
    pub sin_port: in_port_t,
    pub sin_addr: in_addr,
    pub sin_zero: [libc::c_uchar; 8],
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct in_addr {
    pub s_addr: in_addr_t,
}
pub type in_addr_t = uint32_t;
pub type in_port_t = uint16_t;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct sockaddr_in6 {
    pub sin6_family: sa_family_t,
    pub sin6_port: in_port_t,
    pub sin6_flowinfo: uint32_t,
    pub sin6_addr: in6_addr,
    pub sin6_scope_id: uint32_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct in6_addr {
    pub __in6_u: C2RustUnnamed_0,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed_0 {
    pub __u6_addr8: [uint8_t; 16],
    pub __u6_addr16: [uint16_t; 8],
    pub __u6_addr32: [uint32_t; 4],
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct server_config {
    pub max_request_field_size: uint32_t,
    pub log_request_header_on_error: libc::c_uchar,
    pub http_header_strict: libc::c_uchar,
    pub http_host_strict: libc::c_uchar,
    pub http_host_normalize: libc::c_uchar,
    pub http_method_get_body: libc::c_uchar,
    pub high_precision_timestamps: libc::c_uchar,
    pub h2proto: libc::c_uchar,
    pub absolute_dir_redirect: libc::c_uchar,
    pub http_url_normalize: libc::c_ushort,
    pub max_worker: libc::c_ushort,
    pub max_fds: libc::c_ushort,
    pub max_conns: libc::c_ushort,
    pub port: libc::c_ushort,
    pub upload_temp_file_size: libc::c_uint,
    pub upload_tempdirs: *mut array,
    pub dont_daemonize: libc::c_uchar,
    pub preflight_check: libc::c_uchar,
    pub enable_cores: libc::c_uchar,
    pub compat_module_load: libc::c_uchar,
    pub config_deprecated: libc::c_uchar,
    pub config_unsupported: libc::c_uchar,
    pub systemd_socket_activation: libc::c_uchar,
    pub errorlog_use_syslog: libc::c_uchar,
    pub syslog_facility: *const buffer,
    pub bindhost: *const buffer,
    pub changeroot: *const buffer,
    pub username: *const buffer,
    pub groupname: *const buffer,
    pub network_backend: *const buffer,
    pub feature_flags: *const array,
    pub event_handler: *const libc::c_char,
    pub modules_dir: *const libc::c_char,
    pub pid_file: *mut buffer,
    pub modules: *mut array,
    pub config_touched: *mut array,
    pub empty_array: array,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct array {
    pub data: *mut *mut data_unset,
    pub sorted: *mut *mut data_unset,
    pub used: uint32_t,
    pub size: uint32_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct data_unset {
    pub key: buffer,
    pub fn_0: *const data_methods,
    pub type_0: data_type_t,
}
pub type data_type_t = libc::c_uint;
pub const TYPE_OTHER: data_type_t = 4;
pub const TYPE_CONFIG: data_type_t = 3;
pub const TYPE_INTEGER: data_type_t = 2;
pub const TYPE_ARRAY: data_type_t = 1;
pub const TYPE_STRING: data_type_t = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct data_methods {
    pub copy: Option<unsafe extern "C" fn(*const data_unset) -> *mut data_unset>,
    pub free: Option<unsafe extern "C" fn(*mut data_unset) -> ()>,
    pub insert_dup: Option<unsafe extern "C" fn(*mut data_unset, *mut data_unset) -> ()>,
}
pub type log_error_st = fdlog_st;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct fdlog_st {
    pub mode: C2RustUnnamed_1,
    pub fd: libc::c_int,
    pub b: buffer,
    pub fn_0: *const libc::c_char,
}
pub type C2RustUnnamed_1 = libc::c_uint;
pub const FDLOG_PIPE: C2RustUnnamed_1 = 3;
pub const FDLOG_SYSLOG: C2RustUnnamed_1 = 2;
pub const FDLOG_FD: C2RustUnnamed_1 = 1;
pub const FDLOG_FILE: C2RustUnnamed_1 = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct connection {
    pub request: request_st,
    pub h2: *mut h2con,
    pub fd: libc::c_int,
    pub fdn: *mut fdnode,
    pub jqnext: *mut connection,
    pub is_readable: libc::c_schar,
    pub is_writable: libc::c_schar,
    pub is_ssl_sock: libc::c_char,
    pub traffic_limit_reached: libc::c_char,
    pub revents_err: uint16_t,
    pub proto_default_port: uint16_t,
    pub write_queue: *mut chunkqueue,
    pub read_queue: *mut chunkqueue,
    pub bytes_written: off_t,
    pub bytes_written_cur_second: off_t,
    pub bytes_read: off_t,
    pub network_write:
        Option<unsafe extern "C" fn(*mut connection, *mut chunkqueue, off_t) -> libc::c_int>,
    pub network_read:
        Option<unsafe extern "C" fn(*mut connection, *mut chunkqueue, off_t) -> libc::c_int>,
    pub reqbody_read: Option<unsafe extern "C" fn(*mut request_st) -> handler_t>,
    pub srv: *mut server,
    pub plugin_slots: *mut libc::c_void,
    pub plugin_ctx: *mut *mut libc::c_void,
    pub config_data_base: *mut libc::c_void,
    pub dst_addr: sock_addr,
    pub dst_addr_buf: buffer,
    pub srv_socket: *const server_socket,
    pub read_idle_ts: unix_time64_t,
    pub close_timeout_ts: unix_time64_t,
    pub write_request_ts: unix_time64_t,
    pub connection_start: unix_time64_t,
    pub request_count: uint32_t,
    pub keep_alive_idle: libc::c_int,
    pub next: *mut connection,
    pub prev: *mut connection,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct request_st {
    pub state: request_state_t,
    pub http_status: libc::c_int,
    pub h2state: uint32_t,
    pub h2id: uint32_t,
    pub h2_rwin: int32_t,
    pub h2_swin: int32_t,
    pub http_method: http_method_t,
    pub http_version: http_version_t,
    pub handler_module: *const plugin,
    pub plugin_ctx: *mut *mut libc::c_void,
    pub con: *mut connection,
    pub conditional_is_valid: uint32_t,
    pub cond_cache: *mut cond_cache_t,
    pub cond_match: *mut *mut cond_match_t,
    pub cond_match_data: *mut cond_match_t,
    pub conf: request_config,
    pub rqst_header_len: uint32_t,
    pub rqst_htags: uint64_t,
    pub rqst_headers: array,
    pub uri: request_uri,
    pub physical: physical,
    pub env: array,
    pub reqbody_length: off_t,
    pub te_chunked: off_t,
    pub resp_body_scratchpad: off_t,
    pub http_host: *mut buffer,
    pub server_name: *const buffer,
    pub target: buffer,
    pub target_orig: buffer,
    pub pathinfo: buffer,
    pub server_name_buf: buffer,
    pub resp_header_len: uint32_t,
    pub resp_htags: uint64_t,
    pub resp_headers: array,
    pub resp_body_finished: libc::c_char,
    pub resp_body_started: libc::c_char,
    pub resp_send_chunked: libc::c_char,
    pub resp_decode_chunked: libc::c_char,
    pub resp_header_repeated: libc::c_char,
    pub loops_per_request: libc::c_char,
    pub keep_alive: int8_t,
    pub async_callback: libc::c_char,
    pub tmp_buf: *mut buffer,
    pub gw_dechunk: *mut response_dechunk,
    pub bytes_written_ckpt: off_t,
    pub bytes_read_ckpt: off_t,
    pub start_hp: unix_timespec64_t,
    pub error_handler_saved_status: libc::c_int,
    pub error_handler_saved_method: http_method_t,
    pub write_queue: chunkqueue,
    pub read_queue: chunkqueue,
    pub reqbody_queue: chunkqueue,
    pub tmp_sce: *mut stat_cache_entry,
    pub cond_captures: libc::c_int,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct stat_cache_entry {
    pub name: buffer,
    pub stat_ts: unix_time64_t,
    pub fd: libc::c_int,
    pub refcnt: libc::c_int,
    pub fam_dir: *mut libc::c_void,
    pub etag: buffer,
    pub content_type: buffer,
    pub st: stat,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct stat {
    pub st_dev: __dev_t,
    pub st_ino: __ino_t,
    pub st_nlink: __nlink_t,
    pub st_mode: __mode_t,
    pub st_uid: __uid_t,
    pub st_gid: __gid_t,
    pub __pad0: libc::c_int,
    pub st_rdev: __dev_t,
    pub st_size: __off_t,
    pub st_blksize: __blksize_t,
    pub st_blocks: __blkcnt_t,
    pub st_atim: timespec,
    pub st_mtim: timespec,
    pub st_ctim: timespec,
    pub __glibc_reserved: [__syscall_slong_t; 3],
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct chunkqueue {
    pub first: *mut chunk,
    pub last: *mut chunk,
    pub bytes_in: off_t,
    pub bytes_out: off_t,
    pub tempdirs: *const array,
    pub upload_temp_file_size: off_t,
    pub tempdir_idx: libc::c_uint,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct chunk {
    pub next: *mut chunk,
    pub type_0: C2RustUnnamed_4,
    pub mem: *mut buffer,
    pub offset: off_t,
    pub file: C2RustUnnamed_2,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_2 {
    pub length: off_t,
    pub fd: libc::c_int,
    pub is_temp: libc::c_int,
    pub mmap: C2RustUnnamed_3,
    pub ref_0: *mut libc::c_void,
    pub refchg: Option<unsafe extern "C" fn(*mut libc::c_void, libc::c_int) -> ()>,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_3 {
    pub start: *mut libc::c_char,
    pub length: size_t,
    pub offset: off_t,
}
pub type C2RustUnnamed_4 = libc::c_uint;
pub const FILE_CHUNK: C2RustUnnamed_4 = 1;
pub const MEM_CHUNK: C2RustUnnamed_4 = 0;
pub type http_method_t = libc::c_int;
pub const HTTP_METHOD_VERSION_CONTROL: http_method_t = 37;
pub const HTTP_METHOD_UPDATEREDIRECTREF: http_method_t = 36;
pub const HTTP_METHOD_UPDATE: http_method_t = 35;
pub const HTTP_METHOD_UNLOCK: http_method_t = 34;
pub const HTTP_METHOD_UNLINK: http_method_t = 33;
pub const HTTP_METHOD_UNCHECKOUT: http_method_t = 32;
pub const HTTP_METHOD_UNBIND: http_method_t = 31;
pub const HTTP_METHOD_SEARCH: http_method_t = 30;
pub const HTTP_METHOD_REPORT: http_method_t = 29;
pub const HTTP_METHOD_REBIND: http_method_t = 28;
pub const HTTP_METHOD_PROPPATCH: http_method_t = 27;
pub const HTTP_METHOD_PROPFIND: http_method_t = 26;
pub const HTTP_METHOD_PATCH: http_method_t = 25;
pub const HTTP_METHOD_ORDERPATCH: http_method_t = 24;
pub const HTTP_METHOD_MOVE: http_method_t = 23;
pub const HTTP_METHOD_MKWORKSPACE: http_method_t = 22;
pub const HTTP_METHOD_MKREDIRECTREF: http_method_t = 21;
pub const HTTP_METHOD_MKCOL: http_method_t = 20;
pub const HTTP_METHOD_MKCALENDAR: http_method_t = 19;
pub const HTTP_METHOD_MKACTIVITY: http_method_t = 18;
pub const HTTP_METHOD_MERGE: http_method_t = 17;
pub const HTTP_METHOD_LOCK: http_method_t = 16;
pub const HTTP_METHOD_LINK: http_method_t = 15;
pub const HTTP_METHOD_LABEL: http_method_t = 14;
pub const HTTP_METHOD_COPY: http_method_t = 13;
pub const HTTP_METHOD_CHECKOUT: http_method_t = 12;
pub const HTTP_METHOD_CHECKIN: http_method_t = 11;
pub const HTTP_METHOD_BIND: http_method_t = 10;
pub const HTTP_METHOD_BASELINE_CONTROL: http_method_t = 9;
pub const HTTP_METHOD_ACL: http_method_t = 8;
pub const HTTP_METHOD_TRACE: http_method_t = 7;
pub const HTTP_METHOD_OPTIONS: http_method_t = 6;
pub const HTTP_METHOD_CONNECT: http_method_t = 5;
pub const HTTP_METHOD_DELETE: http_method_t = 4;
pub const HTTP_METHOD_PUT: http_method_t = 3;
pub const HTTP_METHOD_POST: http_method_t = 2;
pub const HTTP_METHOD_HEAD: http_method_t = 1;
pub const HTTP_METHOD_GET: http_method_t = 0;
pub const HTTP_METHOD_UNSET: http_method_t = -1;
pub const HTTP_METHOD_PRI: http_method_t = -2;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct response_dechunk {
    pub gw_chunked: off_t,
    pub b: buffer,
    pub done: libc::c_int,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct physical {
    pub path: buffer,
    pub basedir: buffer,
    pub doc_root: buffer,
    pub rel_path: buffer,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct request_uri {
    pub scheme: buffer,
    pub authority: buffer,
    pub path: buffer,
    pub query: buffer,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct request_config {
    pub http_parseopts: libc::c_uint,
    pub max_request_field_size: uint32_t,
    pub mimetypes: *const array,
    pub document_root: *const buffer,
    pub server_name: *const buffer,
    pub server_tag: *const buffer,
    pub errh: *mut fdlog_st,
    pub max_request_size: libc::c_uint,
    pub max_keep_alive_requests: libc::c_ushort,
    pub max_keep_alive_idle: libc::c_ushort,
    pub max_read_idle: libc::c_ushort,
    pub max_write_idle: libc::c_ushort,
    pub stream_request_body: libc::c_ushort,
    pub stream_response_body: libc::c_ushort,
    pub high_precision_timestamps: libc::c_uchar,
    pub allow_http11: libc::c_uchar,
    pub follow_symlink: libc::c_uchar,
    pub etag_flags: libc::c_uchar,
    pub force_lowercase_filenames: libc::c_uchar,
    pub use_xattr: libc::c_uchar,
    pub range_requests: libc::c_uchar,
    pub error_intercept: libc::c_uchar,
    pub h2proto: libc::c_uchar,
    pub log_file_not_found: libc::c_uchar,
    pub log_request_header: libc::c_uchar,
    pub log_request_handling: libc::c_uchar,
    pub log_response_header: libc::c_uchar,
    pub log_condition_handling: libc::c_uchar,
    pub log_timeouts: libc::c_uchar,
    pub log_state_handling: libc::c_uchar,
    pub log_request_header_on_error: libc::c_uchar,
    pub bytes_per_second: libc::c_uint,
    pub global_bytes_per_second: libc::c_uint,
    pub global_bytes_per_second_cnt_ptr: *mut off_t,
    pub error_handler: *const buffer,
    pub error_handler_404: *const buffer,
    pub errorfile_prefix: *const buffer,
    pub serrh: *mut fdlog_st,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct cond_match_t {
    pub comp_value: *const buffer,
    pub match_data: *mut pcre2_real_match_data_8,
    pub captures: libc::c_int,
    pub matches: *mut libc::c_void,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct cond_cache_t {
    pub result: int8_t,
    pub local_result: int8_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct plugin {
    pub data: *mut libc::c_void,
    pub handle_uri_raw:
        Option<unsafe extern "C" fn(*mut request_st, *mut libc::c_void) -> handler_t>,
    pub handle_uri_clean:
        Option<unsafe extern "C" fn(*mut request_st, *mut libc::c_void) -> handler_t>,
    pub handle_docroot:
        Option<unsafe extern "C" fn(*mut request_st, *mut libc::c_void) -> handler_t>,
    pub handle_physical:
        Option<unsafe extern "C" fn(*mut request_st, *mut libc::c_void) -> handler_t>,
    pub handle_request_env:
        Option<unsafe extern "C" fn(*mut request_st, *mut libc::c_void) -> handler_t>,
    pub handle_request_done:
        Option<unsafe extern "C" fn(*mut request_st, *mut libc::c_void) -> handler_t>,
    pub handle_subrequest_start:
        Option<unsafe extern "C" fn(*mut request_st, *mut libc::c_void) -> handler_t>,
    pub handle_subrequest:
        Option<unsafe extern "C" fn(*mut request_st, *mut libc::c_void) -> handler_t>,
    pub handle_response_start:
        Option<unsafe extern "C" fn(*mut request_st, *mut libc::c_void) -> handler_t>,
    pub handle_request_reset:
        Option<unsafe extern "C" fn(*mut request_st, *mut libc::c_void) -> handler_t>,
    pub handle_connection_accept:
        Option<unsafe extern "C" fn(*mut connection, *mut libc::c_void) -> handler_t>,
    pub handle_connection_shut_wr:
        Option<unsafe extern "C" fn(*mut connection, *mut libc::c_void) -> handler_t>,
    pub handle_connection_close:
        Option<unsafe extern "C" fn(*mut connection, *mut libc::c_void) -> handler_t>,
    pub handle_trigger: Option<unsafe extern "C" fn(*mut server, *mut libc::c_void) -> handler_t>,
    pub handle_sighup: Option<unsafe extern "C" fn(*mut server, *mut libc::c_void) -> handler_t>,
    pub handle_waitpid: Option<
        unsafe extern "C" fn(*mut server, *mut libc::c_void, pid_t, libc::c_int) -> handler_t,
    >,
    pub init: Option<unsafe extern "C" fn() -> *mut libc::c_void>,
    pub priv_defaults: Option<unsafe extern "C" fn(*mut server, *mut libc::c_void) -> handler_t>,
    pub set_defaults: Option<unsafe extern "C" fn(*mut server, *mut libc::c_void) -> handler_t>,
    pub worker_init: Option<unsafe extern "C" fn(*mut server, *mut libc::c_void) -> handler_t>,
    pub cleanup: Option<unsafe extern "C" fn(*mut libc::c_void) -> ()>,
    pub name: *const libc::c_char,
    pub version: size_t,
    pub lib: *mut libc::c_void,
}
pub type http_version_t = libc::c_int;
pub const HTTP_VERSION_2: http_version_t = 2;
pub const HTTP_VERSION_1_1: http_version_t = 1;
pub const HTTP_VERSION_1_0: http_version_t = 0;
pub const HTTP_VERSION_UNSET: http_version_t = -1;
pub type request_state_t = libc::c_uint;
pub const CON_STATE_CLOSE: request_state_t = 10;
pub const CON_STATE_ERROR: request_state_t = 9;
pub const CON_STATE_RESPONSE_END: request_state_t = 8;
pub const CON_STATE_WRITE: request_state_t = 7;
pub const CON_STATE_RESPONSE_START: request_state_t = 6;
pub const CON_STATE_HANDLE_REQUEST: request_state_t = 5;
pub const CON_STATE_READ_POST: request_state_t = 4;
pub const CON_STATE_REQUEST_END: request_state_t = 3;
pub const CON_STATE_READ: request_state_t = 2;
pub const CON_STATE_REQUEST_START: request_state_t = 1;
pub const CON_STATE_CONNECT: request_state_t = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct h2con {
    pub r: [*mut request_st; 8],
    pub rused: uint32_t,
    pub h2_cid: uint32_t,
    pub h2_sid: uint32_t,
    pub sent_goaway: int32_t,
    pub sent_settings: unix_time64_t,
    pub s_header_table_size: uint32_t,
    pub s_enable_push: uint32_t,
    pub s_max_concurrent_streams: uint32_t,
    pub s_initial_window_size: int32_t,
    pub s_max_frame_size: uint32_t,
    pub s_max_header_list_size: uint32_t,
    pub decoder: lshpack_dec,
    pub encoder: lshpack_enc,
    pub half_closed_ts: unix_time64_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct lshpack_enc {
    pub hpe_cur_capacity: libc::c_uint,
    pub hpe_max_capacity: libc::c_uint,
    pub hpe_next_id: libc::c_uint,
    pub hpe_nelem: libc::c_uint,
    pub hpe_nbits: libc::c_uint,
    pub hpe_all_entries: lshpack_enc_head,
    pub hpe_buckets: *mut lshpack_double_enc_head,
    pub hpe_hist_buf: *mut uint32_t,
    pub hpe_hist_size: libc::c_uint,
    pub hpe_hist_idx: libc::c_uint,
    pub hpe_hist_wrapped: libc::c_int,
    pub hpe_flags: C2RustUnnamed_5,
}
pub type C2RustUnnamed_5 = libc::c_uint;
pub const LSHPACK_ENC_USE_HIST: C2RustUnnamed_5 = 1;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct lshpack_enc_head {
    pub stqh_first: *mut lshpack_enc_table_entry,
    pub stqh_last: *mut *mut lshpack_enc_table_entry,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct lshpack_dec {
    pub hpd_dyn_table: lshpack_arr,
    pub hpd_max_capacity: libc::c_uint,
    pub hpd_cur_max_capacity: libc::c_uint,
    pub hpd_cur_capacity: libc::c_uint,
    pub hpd_state: libc::c_uint,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct lshpack_arr {
    pub nalloc: libc::c_uint,
    pub nelem: libc::c_uint,
    pub off: libc::c_uint,
    pub els: *mut uintptr_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct data_string {
    pub key: buffer,
    pub fn_0: *const data_methods,
    pub type_0: data_type_t,
    pub ext: libc::c_int,
    pub value: buffer,
}
pub type socklen_t = __socklen_t;
pub type __socket_type = libc::c_uint;
pub const SOCK_NONBLOCK: __socket_type = 2048;
pub const SOCK_CLOEXEC: __socket_type = 524288;
pub const SOCK_PACKET: __socket_type = 10;
pub const SOCK_DCCP: __socket_type = 6;
pub const SOCK_SEQPACKET: __socket_type = 5;
pub const SOCK_RDM: __socket_type = 4;
pub const SOCK_RAW: __socket_type = 3;
pub const SOCK_DGRAM: __socket_type = 2;
pub const SOCK_STREAM: __socket_type = 1;
pub type C2RustUnnamed_6 = libc::c_uint;
pub const SHUT_RDWR: C2RustUnnamed_6 = 2;
pub const SHUT_WR: C2RustUnnamed_6 = 1;
pub const SHUT_RD: C2RustUnnamed_6 = 0;
pub type C2RustUnnamed_7 = libc::c_uint;
pub const IPPROTO_MAX: C2RustUnnamed_7 = 263;
pub const IPPROTO_MPTCP: C2RustUnnamed_7 = 262;
pub const IPPROTO_RAW: C2RustUnnamed_7 = 255;
pub const IPPROTO_ETHERNET: C2RustUnnamed_7 = 143;
pub const IPPROTO_MPLS: C2RustUnnamed_7 = 137;
pub const IPPROTO_UDPLITE: C2RustUnnamed_7 = 136;
pub const IPPROTO_SCTP: C2RustUnnamed_7 = 132;
pub const IPPROTO_COMP: C2RustUnnamed_7 = 108;
pub const IPPROTO_PIM: C2RustUnnamed_7 = 103;
pub const IPPROTO_ENCAP: C2RustUnnamed_7 = 98;
pub const IPPROTO_BEETPH: C2RustUnnamed_7 = 94;
pub const IPPROTO_MTP: C2RustUnnamed_7 = 92;
pub const IPPROTO_AH: C2RustUnnamed_7 = 51;
pub const IPPROTO_ESP: C2RustUnnamed_7 = 50;
pub const IPPROTO_GRE: C2RustUnnamed_7 = 47;
pub const IPPROTO_RSVP: C2RustUnnamed_7 = 46;
pub const IPPROTO_IPV6: C2RustUnnamed_7 = 41;
pub const IPPROTO_DCCP: C2RustUnnamed_7 = 33;
pub const IPPROTO_TP: C2RustUnnamed_7 = 29;
pub const IPPROTO_IDP: C2RustUnnamed_7 = 22;
pub const IPPROTO_UDP: C2RustUnnamed_7 = 17;
pub const IPPROTO_PUP: C2RustUnnamed_7 = 12;
pub const IPPROTO_EGP: C2RustUnnamed_7 = 8;
pub const IPPROTO_TCP: C2RustUnnamed_7 = 6;
pub const IPPROTO_IPIP: C2RustUnnamed_7 = 4;
pub const IPPROTO_IGMP: C2RustUnnamed_7 = 2;
pub const IPPROTO_ICMP: C2RustUnnamed_7 = 1;
pub const IPPROTO_IP: C2RustUnnamed_7 = 0;
pub type request_h2error_t = libc::c_uint;
pub const H2_E_HTTP_1_1_REQUIRED: request_h2error_t = 13;
pub const H2_E_INADEQUATE_SECURITY: request_h2error_t = 12;
pub const H2_E_ENHANCE_YOUR_CALM: request_h2error_t = 11;
pub const H2_E_CONNECT_ERROR: request_h2error_t = 10;
pub const H2_E_COMPRESSION_ERROR: request_h2error_t = 9;
pub const H2_E_CANCEL: request_h2error_t = 8;
pub const H2_E_REFUSED_STREAM: request_h2error_t = 7;
pub const H2_E_FRAME_SIZE_ERROR: request_h2error_t = 6;
pub const H2_E_STREAM_CLOSED: request_h2error_t = 5;
pub const H2_E_SETTINGS_TIMEOUT: request_h2error_t = 4;
pub const H2_E_FLOW_CONTROL_ERROR: request_h2error_t = 3;
pub const H2_E_INTERNAL_ERROR: request_h2error_t = 2;
pub const H2_E_PROTOCOL_ERROR: request_h2error_t = 1;
pub const H2_E_NO_ERROR: request_h2error_t = 0;
pub const COMP_HTTP_REMOTE_IP: C2RustUnnamed_9 = 8;
pub const COMP_SERVER_SOCKET: C2RustUnnamed_9 = 1;
pub const HTTP_HEADER_UPGRADE: http_header_e = 49;
pub type http_header_e = libc::c_uint;
pub const HTTP_HEADER_X_XSS_PROTECTION: http_header_e = 58;
pub const HTTP_HEADER_X_FRAME_OPTIONS: http_header_e = 57;
pub const HTTP_HEADER_X_FORWARDED_PROTO: http_header_e = 56;
pub const HTTP_HEADER_X_FORWARDED_FOR: http_header_e = 55;
pub const HTTP_HEADER_X_CONTENT_TYPE_OPTIONS: http_header_e = 54;
pub const HTTP_HEADER_WWW_AUTHENTICATE: http_header_e = 53;
pub const HTTP_HEADER_VARY: http_header_e = 52;
pub const HTTP_HEADER_USER_AGENT: http_header_e = 51;
pub const HTTP_HEADER_UPGRADE_INSECURE_REQUESTS: http_header_e = 50;
pub const HTTP_HEADER_TRANSFER_ENCODING: http_header_e = 48;
pub const HTTP_HEADER_TE: http_header_e = 47;
pub const HTTP_HEADER_STRICT_TRANSPORT_SECURITY: http_header_e = 46;
pub const HTTP_HEADER_STATUS: http_header_e = 45;
pub const HTTP_HEADER_SET_COOKIE: http_header_e = 44;
pub const HTTP_HEADER_SERVER: http_header_e = 43;
pub const HTTP_HEADER_REFERRER_POLICY: http_header_e = 42;
pub const HTTP_HEADER_REFERER: http_header_e = 41;
pub const HTTP_HEADER_RANGE: http_header_e = 40;
pub const HTTP_HEADER_PRAGMA: http_header_e = 39;
pub const HTTP_HEADER_P3P: http_header_e = 38;
pub const HTTP_HEADER_ONION_LOCATION: http_header_e = 37;
pub const HTTP_HEADER_LOCATION: http_header_e = 36;
pub const HTTP_HEADER_LINK: http_header_e = 35;
pub const HTTP_HEADER_LAST_MODIFIED: http_header_e = 34;
pub const HTTP_HEADER_IF_UNMODIFIED_SINCE: http_header_e = 33;
pub const HTTP_HEADER_IF_RANGE: http_header_e = 32;
pub const HTTP_HEADER_IF_NONE_MATCH: http_header_e = 31;
pub const HTTP_HEADER_IF_MODIFIED_SINCE: http_header_e = 30;
pub const HTTP_HEADER_IF_MATCH: http_header_e = 29;
pub const HTTP_HEADER_HTTP2_SETTINGS: http_header_e = 28;
pub const HTTP_HEADER_HOST: http_header_e = 27;
pub const HTTP_HEADER_FORWARDED: http_header_e = 26;
pub const HTTP_HEADER_EXPIRES: http_header_e = 25;
pub const HTTP_HEADER_EXPECT_CT: http_header_e = 24;
pub const HTTP_HEADER_EXPECT: http_header_e = 23;
pub const HTTP_HEADER_ETAG: http_header_e = 22;
pub const HTTP_HEADER_DNT: http_header_e = 21;
pub const HTTP_HEADER_DATE: http_header_e = 20;
pub const HTTP_HEADER_COOKIE: http_header_e = 19;
pub const HTTP_HEADER_CONTENT_TYPE: http_header_e = 18;
pub const HTTP_HEADER_CONTENT_SECURITY_POLICY: http_header_e = 17;
pub const HTTP_HEADER_CONTENT_RANGE: http_header_e = 16;
pub const HTTP_HEADER_CONTENT_LOCATION: http_header_e = 15;
pub const HTTP_HEADER_CONTENT_LENGTH: http_header_e = 14;
pub const HTTP_HEADER_CONTENT_ENCODING: http_header_e = 13;
pub const HTTP_HEADER_CONNECTION: http_header_e = 12;
pub const HTTP_HEADER_CACHE_CONTROL: http_header_e = 11;
pub const HTTP_HEADER_AUTHORIZATION: http_header_e = 10;
pub const HTTP_HEADER_ALT_USED: http_header_e = 9;
pub const HTTP_HEADER_ALT_SVC: http_header_e = 8;
pub const HTTP_HEADER_ALLOW: http_header_e = 7;
pub const HTTP_HEADER_AGE: http_header_e = 6;
pub const HTTP_HEADER_ACCESS_CONTROL_ALLOW_ORIGIN: http_header_e = 5;
pub const HTTP_HEADER_ACCEPT_RANGES: http_header_e = 4;
pub const HTTP_HEADER_ACCEPT_LANGUAGE: http_header_e = 3;
pub const HTTP_HEADER_ACCEPT_ENCODING: http_header_e = 2;
pub const HTTP_HEADER_ACCEPT: http_header_e = 1;
pub const HTTP_HEADER_OTHER: http_header_e = 0;
pub const H2_STATE_HALF_CLOSED_REMOTE: C2RustUnnamed_8 = 5;
pub type C2RustUnnamed_8 = libc::c_uint;
pub const H2_STATE_CLOSED: C2RustUnnamed_8 = 6;
pub const H2_STATE_HALF_CLOSED_LOCAL: C2RustUnnamed_8 = 4;
pub const H2_STATE_OPEN: C2RustUnnamed_8 = 3;
pub const H2_STATE_RESERVED_REMOTE: C2RustUnnamed_8 = 2;
pub const H2_STATE_RESERVED_LOCAL: C2RustUnnamed_8 = 1;
pub const H2_STATE_IDLE: C2RustUnnamed_8 = 0;
pub type C2RustUnnamed_9 = libc::c_uint;
pub const COMP_LAST_ELEMENT: C2RustUnnamed_9 = 13;
pub const COMP_HTTP_REQUEST_HEADER: C2RustUnnamed_9 = 12;
pub const COMP_HTTP_REQUEST_METHOD: C2RustUnnamed_9 = 11;
pub const COMP_HTTP_SCHEME: C2RustUnnamed_9 = 10;
pub const COMP_HTTP_QUERY_STRING: C2RustUnnamed_9 = 9;
pub const COMP_HTTP_COOKIE: C2RustUnnamed_9 = 7;
pub const COMP_HTTP_LANGUAGE: C2RustUnnamed_9 = 6;
pub const COMP_HTTP_USER_AGENT: C2RustUnnamed_9 = 5;
pub const COMP_HTTP_REFERER: C2RustUnnamed_9 = 4;
pub const COMP_HTTP_HOST: C2RustUnnamed_9 = 3;
pub const COMP_HTTP_URL: C2RustUnnamed_9 = 2;
pub const COMP_UNSET: C2RustUnnamed_9 = 0;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct fdevents {
    pub fdarray: *mut *mut fdnode,
}

unsafe extern "C" fn connection_handle_fdevent(
    context: *mut libc::c_void,
    revents: libc::c_int,
) -> handler_t {
    return 1;
}

unsafe extern "C" fn connection_reset(mut con: *mut connection) {
    let r: *mut request_st = &mut (*con).request;
    // request_reset(r);
    (*r).bytes_read_ckpt = 0 as libc::c_int as off_t;
    (*r).bytes_written_ckpt = 0 as libc::c_int as off_t;
    (*con).is_readable = 1 as libc::c_int as libc::c_schar;
    (*con).bytes_written = 0 as libc::c_int as off_t;
    (*con).bytes_written_cur_second = 0 as libc::c_int as off_t;
    (*con).bytes_read = 0 as libc::c_int as off_t;
}

#[no_mangle]
pub unsafe extern "C" fn connection_accepted(
    mut srv: *mut server,
    mut srv_socket: *const server_socket,
    mut cnt_addr: *mut sock_addr,
    mut cnt: libc::c_int,
    fdn: *mut fdnode,         // TODO: remove when casts from c_void are handled
    mut con: *mut connection, // TODO: remove when casts from c_void are handled
) -> *mut connection {
    // let mut con: *mut connection = 0 as *mut connection;
    (*srv).cur_fds += 1;
    (*srv).con_opened += 1;
    con = connections_get_new_connection(srv, con);
    (*con).fd = cnt;
    (*con).fdn = fdevent_register(
        (*srv).ev,
        (*con).fd,
        // Some(
        //     connection_handle_fdevent
        //         as unsafe extern "C" fn(*mut libc::c_void, libc::c_int) -> handler_t,
        // ),
        // con as *mut libc::c_void,
        fdn,
    );
    // (*con)
    //     .network_read = Some(
    //     connection_read_cq
    //         as unsafe extern "C" fn(
    //             *mut connection,
    //             *mut chunkqueue,
    //             off_t,
    //         ) -> libc::c_int,
    // );
    // (*con)
    //     .network_write = Some(
    //     connection_write_cq
    //         as unsafe extern "C" fn(
    //             *mut connection,
    //             *mut chunkqueue,
    //             off_t,
    //         ) -> libc::c_int,
    // );
    // (*con)
    //     .reqbody_read = Some(
    //     connection_handle_read_post_state
    //         as unsafe extern "C" fn(*mut request_st) -> handler_t,
    // );
    let r: *mut request_st = &mut (*con).request;
    (*r).state = CON_STATE_REQUEST_START;
    // (*con).connection_start = log_monotonic_secs;
    (*con).dst_addr = *cnt_addr;
    // sock_addr_cache_inet_ntop_copy_buffer(
    //     &mut (*con).dst_addr_buf,
    //     &mut (*con).dst_addr,
    // );
    (*con).srv_socket = srv_socket;
    (*con).is_ssl_sock = (*srv_socket).is_ssl as libc::c_char;
    (*con).proto_default_port = 80 as libc::c_int as uint16_t;
    // config_cond_cache_reset(r);
    (*r)
        .conditional_is_valid = ((1 as libc::c_int) << COMP_SERVER_SOCKET as libc::c_int
        | (1 as libc::c_int) << COMP_HTTP_REMOTE_IP as libc::c_int) as uint32_t;
    if HANDLER_GO_ON as libc::c_int as libc::c_uint != 0 // <-- TODO: remove
        // != plugins_call_handle_connection_accept(con) as libc::c_uint
    {
        connection_reset(con);
        connection_close(con);
        return con; // 0 as *mut connection;
    }
    if (*r).http_status < 0 as libc::c_int {
        (*r).state = CON_STATE_WRITE;
    }
    return con;
}

#[cold]
unsafe extern "C" fn connection_init(
    mut srv: *mut server,
    mut con: *mut connection,
) -> *mut connection {
    // let con: *mut connection = calloc(
    //     1 as libc::c_int as libc::c_ulong,
    //     ::std::mem::size_of::<connection>() as libc::c_ulong,
    // ) as *mut connection;
    if con.is_null() {
        // ck_assert_failed(
        //     b"src/connections.c\0" as *const u8 as *const libc::c_char,
        //     500 as libc::c_int as libc::c_uint,
        //     b"((void*)0) != con\0" as *const u8 as *const libc::c_char,
        // );
    }
    (*con).srv = srv;
    (*con).plugin_slots = (*srv).plugin_slots;
    (*con).config_data_base = (*srv).config_data_base;
    let r: *mut request_st = &mut (*con).request;
    // request_init_data(r, con, srv);
    (*con).write_queue = &mut (*r).write_queue;
    (*con).read_queue = &mut (*r).read_queue;
    // (*con).plugin_ctx = calloc(
    //     1 as libc::c_int as libc::c_ulong,
    //     (((*srv).plugins.used).wrapping_add(1 as libc::c_int as libc::c_uint) as libc::c_ulong)
    //         .wrapping_mul(::std::mem::size_of::<*mut libc::c_void>() as libc::c_ulong),
    // ) as *mut *mut libc::c_void;
    if ((*con).plugin_ctx).is_null() {
        // ck_assert_failed(
        //     b"src/connections.c\0" as *const u8 as *const libc::c_char,
        //     513 as libc::c_int as libc::c_uint,
        //     b"((void*)0) != con->plugin_ctx\0" as *const u8 as *const libc::c_char,
        // );
    }
    return con;
}

unsafe extern "C" fn connections_get_new_connection(
    mut srv: *mut server,
    mut con: *mut connection,
) -> *mut connection {
    // let mut con: *mut connection = 0 as *mut connection;
    // (*srv).lim_conns = ((*srv).lim_conns).wrapping_sub(1);
    if !((*srv).conns_pool).is_null() {
        con = (*srv).conns_pool;
        (*srv).conns_pool = (*con).next;
    } else {
        con = connection_init(srv, con);
        connection_reset(con);
    }
    (*con).next = (*srv).conns;
    if !((*con).next).is_null() {
        (*(*con).next).prev = con;
    }
    (*srv).conns = con;
    return (*srv).conns;
}

#[no_mangle]
pub unsafe extern "C" fn fdevent_register(
    mut ev: *mut fdevents,
    mut fd: libc::c_int,
    // mut handler: fdevent_handler,
    // mut ctx: *mut libc::c_void,
    fdn: *mut fdnode,
) -> *mut fdnode {
    let ref mut fresh0 = *((*ev).fdarray).offset(fd as isize);
    *fresh0 = fdnode_init(fdn);
    let mut fdn: *mut fdnode = *fresh0;
    // (*fdn).handler = handler;
    (*fdn).fd = fd;
    // (*fdn).ctx = ctx;
    (*fdn).events = 0 as libc::c_int;
    (*fdn).fde_ndx = -(1 as libc::c_int);
    return fdn;
}

unsafe extern "C" fn fdnode_init(fdn: *mut fdnode) -> *mut fdnode {
    // let fdn: *mut fdnode = calloc(
    //     1 as libc::c_int as libc::c_ulong,
    //     ::std::mem::size_of::<fdnode>() as libc::c_ulong,
    // ) as *mut fdnode;
    if fdn.is_null() {
        // ck_assert_failed(
        //     b"src/fdevent_fdnode.c\0" as *const u8 as *const libc::c_char,
        //     17 as libc::c_int as libc::c_uint,
        //     b"((void*)0) != fdn\0" as *const u8 as *const libc::c_char,
        // );
    }

    fdn
}

unsafe extern "C" fn connection_del(mut srv: *mut server, mut con: *mut connection) {
    if !((*con).next).is_null() {
        (*(*con).next).prev = (*con).prev;
    }
    if !((*con).prev).is_null() {
        (*(*con).prev).next = (*con).next;
    } else {
        (*srv).conns = (*con).next;
    }
    (*con).prev = con; // 0 as *mut connection;
    (*con).next = (*srv).conns_pool;
    (*srv).conns_pool = con;
    // (*srv).lim_conns = ((*srv).lim_conns).wrapping_add(1);
}

unsafe extern "C" fn connection_close(mut con: *mut connection) {
    if (*con).fd < 0 as libc::c_int {
        (*con).fd = -(*con).fd;
    }
    // plugins_call_handle_connection_close(con);
    let srv: *mut server = (*con).srv;
    let r: *mut request_st = &mut (*con).request;
    // request_reset_ex(r);
    (*r).state = CON_STATE_CONNECT;
    // chunkqueue_reset((*con).read_queue);
    (*con).request_count = 0 as libc::c_int as uint32_t;
    (*con).is_ssl_sock = 0 as libc::c_int as libc::c_char;
    (*con).revents_err = 0 as libc::c_int as uint16_t;
    fdevent_fdnode_event_del((*srv).ev, (*con).fdn);
    fdevent_unregister((*srv).ev, (*con).fd);
    // (*con).fdn = 0 as *mut fdnode;
    // if 0 as libc::c_int == close((*con).fd) {
    //     (*srv).cur_fds -= 1;
    // } else {
    //     log_perror(
    //         (*r).conf.errh,
    //         b"src/connections.c\0" as *const u8 as *const libc::c_char,
    //         101 as libc::c_int as libc::c_uint,
    //         b"(warning) close: %d\0" as *const u8 as *const libc::c_char,
    //         (*con).fd,
    //     );
    // }
    if (*r).conf.log_state_handling != 0 {
        // log_error(
        //     (*r).conf.errh,
        //     b"src/connections.c\0" as *const u8 as *const libc::c_char,
        //     105 as libc::c_int as libc::c_uint,
        //     b"connection closed for fd %d\0" as *const u8 as *const libc::c_char,
        //     (*con).fd,
        // );
    }
    (*con).fd = -(1 as libc::c_int);
    connection_del(srv, con);
}

#[no_mangle]
pub unsafe extern "C" fn fdevent_fdnode_event_del(mut ev: *mut fdevents, mut fdn: *mut fdnode) {
    if !fdn.is_null() {
        fdevent_fdnode_event_unsetter(ev, fdn);
    }
}

unsafe extern "C" fn fdevent_fdnode_event_unsetter(mut ev: *mut fdevents, mut fdn: *mut fdnode) {
    if -(1 as libc::c_int) == (*fdn).fde_ndx {
        return;
    }
    (*fdn).fde_ndx = -(1 as libc::c_int);
    (*fdn).events = 0 as libc::c_int;
}

#[no_mangle]
pub unsafe extern "C" fn fdevent_unregister(mut ev: *mut fdevents, mut fd: libc::c_int) {
    let mut fdn: *mut fdnode = *((*ev).fdarray).offset(fd as isize);
    if fdn as uintptr_t & 0x3 as libc::c_int as libc::c_ulong != 0 {
        return;
    }
    let ref mut fresh1 = *((*ev).fdarray).offset(fd as isize);
    // *fresh1 = 0 as *mut fdnode;
    // fdnode_free(fdn);
}

unsafe extern "C" fn fdnode_free(fdn: *mut libc::c_void) {
    free(fdn /* TODO: handle cast as *mut libc::c_void */);
}

pub unsafe extern "C" fn lighttpd_test() {
    let fdarr = calloc(
        0 as libc::c_int as libc::c_ulong,
        ::std::mem::size_of::<*mut fdnode>() as libc::c_ulong,
    ); // TODO: handle cast as *mut *mut fdnode;
    let fdes = calloc(
        0 as libc::c_int as libc::c_ulong,
        ::std::mem::size_of::<fdevents>() as libc::c_ulong,
    ); // TODO: handle cast as *mut fdevents;
    free(fdarr /* TODO: handle cast as *mut libc::c_void */);
    free(fdes /* TODO: handle cast as *mut libc::c_void */);
}

fn main() {
    unsafe {
        lighttpd_test();
    }
}
