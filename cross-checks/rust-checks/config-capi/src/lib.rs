
extern crate serde;
extern crate serde_yaml;

extern crate cross_check_config as xcfg;

use std::ffi::CStr;
use std::os::raw::{c_char, c_uint};
use std::ptr;
use std::slice;

// Constants shared with C clients
const XCHECK_TYPE_DEFAULT:  c_uint = 0;
const XCHECK_TYPE_DISABLED: c_uint = 1;
const XCHECK_TYPE_FIXED:    c_uint = 2;
const XCHECK_TYPE_DJB2:     c_uint = 3;
const XCHECK_TYPE_AS_TYPE:  c_uint = 4;
const XCHECK_TYPE_CUSTOM:   c_uint = 5;

const XCHECK_TAG_UNKNOWN:         c_uint = 0;
const XCHECK_TAG_FUNCTION_ENTRY:  c_uint = 1;
const XCHECK_TAG_FUNCTION_EXIT:   c_uint = 2;
const XCHECK_TAG_FUNCTION_ARG:    c_uint = 3;
const XCHECK_TAG_FUNCTION_RETURN: c_uint = 4;

const ITEM_KIND_FUNCTION: c_uint = 0;
const ITEM_KIND_STRUCT:   c_uint = 1;
const ITEM_KIND_IMPL:     c_uint = 2;

// Common basic data structures and functions
#[repr(C)]
pub struct StringLenPtr {
    ptr: *const c_char,
    len: c_uint,
}

impl StringLenPtr {
    fn from_str(s: &str) -> StringLenPtr {
        StringLenPtr {
            ptr: s.as_ptr() as *const c_char,
            len: s.len() as c_uint,
        }
    }

    fn from_option_str(s: &Option<String>) -> StringLenPtr {
        let (ptr, len) = match s {
            Some(s) => (s.as_ptr(), s.len()),
            None    => (ptr::null(), 0),
        };
        StringLenPtr {
            ptr: ptr as *const c_char,
            len: len as c_uint,
        }
    }

    unsafe fn to_slice(&self) -> &[u8] {
        slice::from_raw_parts(self.ptr as *const u8, self.len as usize)
    }
}

impl Default for StringLenPtr {
    fn default() -> StringLenPtr {
        StringLenPtr {
            ptr: ptr::null(),
            len: 0,
        }
    }
}

#[repr(C)]
pub struct VecLenPtr<T> {
    ptr: *const T,
    len: c_uint,
}

impl<T> VecLenPtr<T> {
    fn from_slice(v: &[T]) -> VecLenPtr<T> {
        VecLenPtr {
            ptr: v.as_ptr(),
            len: v.len() as c_uint,
        }
    }
}

impl<T> Default for VecLenPtr<T> {
    fn default() -> VecLenPtr<T> {
        VecLenPtr {
            ptr: ptr::null(),
            len: 0,
        }
    }
}

#[inline]
fn option_to_ptr<T>(opt: Option<&T>) -> *const T {
    match opt {
        Some(x) => x,
        None => ptr::null()
    }
}

// Top-level parsing API
pub unsafe extern fn xcfg_config_parse(buf: StringLenPtr) -> *const xcfg::Config {
    let slice = buf.to_slice();
    let cfg = serde_yaml::from_slice(slice)
        .expect(&format!("invalid YAML: '{:?}'", slice));
    Box::into_raw(Box::new(cfg))
}

pub unsafe extern fn xcfg_config_destroy(cfg: *mut xcfg::Config) {
    let cfg = Box::from_raw(cfg);
    drop(cfg);
}

// C API for cross-check types
pub unsafe extern fn xcfg_xcheck_type(xcheck: *const xcfg::XCheckType)
    -> c_uint {
    match *xcheck {
        xcfg::XCheckType::Default   => XCHECK_TYPE_DEFAULT,
        xcfg::XCheckType::None      => XCHECK_TYPE_DISABLED,
        xcfg::XCheckType::Disabled  => XCHECK_TYPE_DISABLED,
        xcfg::XCheckType::Fixed(_)  => XCHECK_TYPE_FIXED,
        xcfg::XCheckType::Djb2(_)   => XCHECK_TYPE_DJB2,
        xcfg::XCheckType::AsType(_) => XCHECK_TYPE_AS_TYPE,
        xcfg::XCheckType::Custom(_) => XCHECK_TYPE_CUSTOM,
    }
}

pub unsafe extern fn xcfg_xcheck_data_u64(xcheck: *const xcfg::XCheckType)
    -> u64 {
    match *xcheck {
        xcfg::XCheckType::Fixed(x)  => x,
        _ => 0,
    }
}

pub unsafe extern fn xcfg_xcheck_data_string(xcheck: *const xcfg::XCheckType)
    -> StringLenPtr {
    match *xcheck {
        xcfg::XCheckType::Djb2(ref s)   |
        xcfg::XCheckType::AsType(ref s) |
        xcfg::XCheckType::Custom(ref s) => StringLenPtr::from_str(s),
        _ => Default::default(),
    }
}

pub unsafe extern fn xcfg_extra_xcheck_tag(extra_xcheck: *const xcfg::ExtraXCheck)
    -> c_uint {
    match (*extra_xcheck).tag {
        xcfg::XCheckTag::Unknown        => XCHECK_TAG_UNKNOWN,
        xcfg::XCheckTag::FunctionEntry  => XCHECK_TAG_FUNCTION_ENTRY,
        xcfg::XCheckTag::FunctionExit   => XCHECK_TAG_FUNCTION_EXIT,
        xcfg::XCheckTag::FunctionArg    => XCHECK_TAG_FUNCTION_ARG,
        xcfg::XCheckTag::FunctionReturn => XCHECK_TAG_FUNCTION_RETURN,
    }
}

pub unsafe extern fn xcfg_extra_xcheck_custom(extra_xcheck: *const xcfg::ExtraXCheck)
    -> StringLenPtr {
    StringLenPtr::from_str(&(*extra_xcheck).custom)
}

// C API for scope configuration
// TODO: should this be guarded by a feature???
pub unsafe extern fn xcfg_scope_stack_new(top_scope: *const xcfg::scopes::ScopeConfig)
    -> *mut xcfg::scopes::ScopeStack {
    let scope_stack = if top_scope.is_null() {
        xcfg::scopes::ScopeStack::new()
    } else {
        xcfg::scopes::ScopeStack::from_scope((*top_scope).clone())
    };
    Box::into_raw(Box::new(scope_stack))
}

pub unsafe extern fn xcfg_scope_stack_destroy(scope_stack: *mut xcfg::scopes::ScopeStack) {
    let scope_stack = Box::from_raw(scope_stack);
    drop(scope_stack);
}

pub unsafe extern fn xcfg_scope_stack_push_file(scope_stack: *mut xcfg::scopes::ScopeStack,
                                                external_config: *const xcfg::Config,
                                                file_name: *const c_char) {
    let file_name = CStr::from_ptr(file_name).to_str().unwrap();
    (*scope_stack).push_file(&*external_config, file_name);
    // TODO: return something???
}

pub unsafe extern fn xcfg_scope_stack_push_item(scope_stack: *mut xcfg::scopes::ScopeStack,
                                                item_kind: c_uint,
                                                file_name: *const c_char,
                                                item_name: *const c_char,
                                                pre_xcfg: *const c_char,
                                                post_xcfg: *const c_char) {
    let item_kind = match item_kind {
        ITEM_KIND_FUNCTION => xcfg::scopes::ItemKind::Function,
        ITEM_KIND_STRUCT   => xcfg::scopes::ItemKind::Struct,
        ITEM_KIND_IMPL     => xcfg::scopes::ItemKind::Impl,
        _ => panic!("unknown item kind: {}", item_kind)
    };
    let file_name = CStr::from_ptr(file_name).to_str().unwrap();
    let item_name = CStr::from_ptr(item_name).to_str().unwrap();
    let pre_xcfg = if pre_xcfg.is_null() { None } else {
        let pre_cstr = CStr::from_ptr(pre_xcfg);
        Some(serde_yaml::from_slice(pre_cstr.to_bytes())
            .expect(&format!("invalid YAML: '{:?}'", pre_cstr)))
    };
    let post_xcfg = if post_xcfg.is_null() { None } else {
        let post_cstr = CStr::from_ptr(post_xcfg);
        Some(serde_yaml::from_slice(post_cstr.to_bytes())
            .expect(&format!("invalid YAML: '{:?}'", post_cstr)))
    };
    (*scope_stack).push_item(item_kind, file_name, item_name, pre_xcfg, post_xcfg);
    // TODO: return something???
}

pub unsafe extern fn xcfg_scope_stack_pop(scope_stack: *mut xcfg::scopes::ScopeStack) {
    (*scope_stack).pop();
    // TODO: return something???
}

pub unsafe extern fn xcfg_scope_stack_pop_multi(scope_stack: *mut xcfg::scopes::ScopeStack,
                                                cnt: usize) {
    (*scope_stack).pop_multi(cnt);
    // TODO: return something???
}

pub unsafe extern fn xcfg_scope_stack_last(scope_stack: *mut xcfg::scopes::ScopeStack)
    -> *mut xcfg::scopes::ScopeConfig {
    (*scope_stack).last_mut()
}

pub unsafe extern fn xcfg_scope_enabled(scope_config: *mut xcfg::scopes::ScopeConfig)
    -> c_uint {
    if (*scope_config).inherited.enabled { 1 } else { 0 }
}

pub unsafe extern fn xcfg_scope_entry(scope_config: *mut xcfg::scopes::ScopeConfig)
    -> *const xcfg::XCheckType {
    &(*scope_config).inherited.entry
}

pub unsafe extern fn xcfg_scope_exit(scope_config: *mut xcfg::scopes::ScopeConfig)
    -> *const xcfg::XCheckType {
    &(*scope_config).inherited.exit
}

pub unsafe extern fn xcfg_scope_all_args(scope_config: *mut xcfg::scopes::ScopeConfig)
    -> *const xcfg::XCheckType {
    &(*scope_config).inherited.all_args
}

pub unsafe extern fn xcfg_scope_ret(scope_config: *mut xcfg::scopes::ScopeConfig)
    -> *const xcfg::XCheckType {
    &(*scope_config).inherited.ret
}

pub unsafe extern fn xcfg_scope_ahasher(scope_config: *mut xcfg::scopes::ScopeConfig)
    -> StringLenPtr {
    StringLenPtr::from_option_str(&(*scope_config).inherited.ahasher)
}

pub unsafe extern fn xcfg_scope_shasher(scope_config: *mut xcfg::scopes::ScopeConfig)
    -> StringLenPtr {
    StringLenPtr::from_option_str(&(*scope_config).inherited.shasher)
}

pub unsafe extern fn xcfg_scope_function_arg(scope_config: *mut xcfg::scopes::ScopeConfig,
                                             arg_name: *const c_char)
    -> *const xcfg::XCheckType {
    match (*scope_config).item {
        xcfg::scopes::ItemConfig::Function(ref f) => {
            let arg_name = CStr::from_ptr(arg_name).to_str().unwrap();
            let arg_index = xcfg::FieldIndex::Str(String::from(arg_name));
            option_to_ptr(f.args.get(&arg_index))
        }
        _ => ptr::null()
    }
}

pub unsafe extern fn xcfg_scope_function_entry_extra(scope_config: *mut xcfg::scopes::ScopeConfig)
    -> VecLenPtr<xcfg::ExtraXCheck> {
    match (*scope_config).item {
        xcfg::scopes::ItemConfig::Function(ref f) =>
            VecLenPtr::from_slice(&f.entry_extra),
        _ => Default::default(),
    }
}

pub unsafe extern fn xcfg_scope_function_exit_extra(scope_config: *mut xcfg::scopes::ScopeConfig)
    -> VecLenPtr<xcfg::ExtraXCheck>{
    match (*scope_config).item {
        xcfg::scopes::ItemConfig::Function(ref f) =>
            VecLenPtr::from_slice(&f.exit_extra),
        _ => Default::default(),
    }
}

pub unsafe extern fn xcfg_scope_struct_custom_hash(scope_config: *mut xcfg::scopes::ScopeConfig)
    -> StringLenPtr {
    match (*scope_config).item {
        xcfg::scopes::ItemConfig::Struct(ref s) =>
            StringLenPtr::from_option_str(&s.custom_hash),
        _ => Default::default(),
    }
}

pub unsafe extern fn xcfg_scope_struct_field_hasher(scope_config: *mut xcfg::scopes::ScopeConfig)
    -> StringLenPtr {
    match (*scope_config).item {
        xcfg::scopes::ItemConfig::Struct(ref s) =>
            StringLenPtr::from_option_str(&s.field_hasher),
        _ => Default::default(),
    }
}

pub unsafe extern fn xcfg_scope_struct_field(scope_config: *mut xcfg::scopes::ScopeConfig,
                                             field_name: *const c_char)
    -> *const xcfg::XCheckType {
    match (*scope_config).item {
        xcfg::scopes::ItemConfig::Struct(ref s) => {
            let field_name = CStr::from_ptr(field_name).to_str().unwrap();
            let field_index = xcfg::FieldIndex::Str(String::from(field_name));
            option_to_ptr(s.fields.get(&field_index))
        }
        _ => ptr::null()
    }
}
