
extern crate serde;
extern crate serde_yaml;

extern crate cross_check_config as xcfg;

use std::os::raw::{c_char, c_uint};
use std::mem;
use std::ptr;
use std::slice;
use std::str;

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
#[derive(Debug)]
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

    unsafe fn to_str(&self) -> &str {
        str::from_utf8(self.to_slice()).unwrap()
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
#[derive(Debug)]
pub struct VecLenPtr<T> {
    ptr: *const T,
    elem_size: c_uint,
    len: c_uint,
}

impl<T> VecLenPtr<T> {
    fn from_slice(v: &[T]) -> VecLenPtr<T> {
        VecLenPtr {
            ptr: v.as_ptr(),
            elem_size: mem::size_of::<T>() as c_uint,
            len: v.len() as c_uint,
        }
    }

    unsafe fn get(&self, idx: usize) -> &T {
        &*self.ptr.offset(idx as isize)
    }
}

impl<T> Default for VecLenPtr<T> {
    fn default() -> VecLenPtr<T> {
        VecLenPtr {
            ptr: ptr::null(),
            elem_size: mem::size_of::<T>() as c_uint,
            len: 0,
        }
    }
}

// Top-level parsing API
#[no_mangle]
pub unsafe extern fn xcfg_config_new()
    -> *const xcfg::Config {
    Box::into_raw(Box::new(xcfg::Config::default()))
}

#[no_mangle]
pub unsafe extern fn xcfg_config_parse(cfg: *mut xcfg::Config, buf: StringLenPtr)
    -> *const xcfg::Config {
    let cfg = Box::from_raw(cfg);
    let second_cfg = serde_yaml::from_slice(buf.to_slice())
        .expect(&format!("invalid YAML: '{:?}'", buf.to_str()));
    let merged_cfg = cfg.merge(second_cfg);
    Box::into_raw(Box::new(merged_cfg))
}

#[no_mangle]
pub unsafe extern fn xcfg_config_destroy(cfg: *mut xcfg::Config) {
    let cfg = Box::from_raw(cfg);
    drop(cfg);
}

// C API for cross-check types
#[no_mangle]
pub unsafe extern fn xcfg_xcheck_type(xcheck: Option<&xcfg::XCheckType>) -> c_uint {
    match xcheck {
        Some(&xcfg::XCheckType::Default)   => XCHECK_TYPE_DEFAULT,
        Some(&xcfg::XCheckType::None)      => XCHECK_TYPE_DISABLED,
        Some(&xcfg::XCheckType::Disabled)  => XCHECK_TYPE_DISABLED,
        Some(&xcfg::XCheckType::Fixed(_))  => XCHECK_TYPE_FIXED,
        Some(&xcfg::XCheckType::Djb2(_))   => XCHECK_TYPE_DJB2,
        Some(&xcfg::XCheckType::AsType(_)) => XCHECK_TYPE_AS_TYPE,
        Some(&xcfg::XCheckType::Custom(_)) => XCHECK_TYPE_CUSTOM,
        None => XCHECK_TYPE_DEFAULT,
    }
}

#[no_mangle]
pub unsafe extern fn xcfg_xcheck_data_u64(xcheck: Option<&xcfg::XCheckType>) -> u64 {
    match xcheck {
        Some(&xcfg::XCheckType::Fixed(x))  => x,
        _ => 0,
    }
}

#[no_mangle]
pub unsafe extern fn xcfg_xcheck_data_string(xcheck: Option<&xcfg::XCheckType>) -> StringLenPtr {
    match xcheck {
        Some(&xcfg::XCheckType::Djb2(ref s))   |
        Some(&xcfg::XCheckType::AsType(ref s)) |
        Some(&xcfg::XCheckType::Custom(ref s)) => StringLenPtr::from_str(s),
        _ => Default::default(),
    }
}

#[no_mangle]
pub unsafe extern fn xcfg_extra_xcheck_tag(extra_xcheck: Option<&xcfg::ExtraXCheck>) -> c_uint {
    match extra_xcheck.unwrap().tag {
        xcfg::XCheckTag::Unknown        => XCHECK_TAG_UNKNOWN,
        xcfg::XCheckTag::FunctionEntry  => XCHECK_TAG_FUNCTION_ENTRY,
        xcfg::XCheckTag::FunctionExit   => XCHECK_TAG_FUNCTION_EXIT,
        xcfg::XCheckTag::FunctionArg    => XCHECK_TAG_FUNCTION_ARG,
        xcfg::XCheckTag::FunctionReturn => XCHECK_TAG_FUNCTION_RETURN,
    }
}

#[no_mangle]
pub unsafe extern fn xcfg_extra_xcheck_custom(extra_xcheck: Option<&xcfg::ExtraXCheck>)
    -> StringLenPtr {
    StringLenPtr::from_str(&extra_xcheck.unwrap().custom)
}

// C API for scope configuration
// TODO: should this be guarded by a feature???
#[no_mangle]
pub unsafe extern fn xcfg_scope_stack_new(top_scope: *const xcfg::scopes::ScopeConfig)
    -> *mut xcfg::scopes::ScopeStack {
    let scope_stack = if top_scope.is_null() {
        xcfg::scopes::ScopeStack::new()
    } else {
        xcfg::scopes::ScopeStack::from_scope((*top_scope).clone())
    };
    Box::into_raw(Box::new(scope_stack))
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_stack_destroy(scope_stack: *mut xcfg::scopes::ScopeStack) {
    let scope_stack = Box::from_raw(scope_stack);
    drop(scope_stack);
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_stack_push_file<'stk>(scope_stack: Option<&'stk mut xcfg::scopes::ScopeStack>,
                                                      external_config: Option<&xcfg::Config>,
                                                      file_name: StringLenPtr)
    -> Option<&'stk xcfg::scopes::ScopeConfig> {
    scope_stack.and_then(|stk| stk.push_file(external_config.unwrap(), file_name.to_str()))
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_stack_push_item<'stk>(scope_stack: Option<&'stk mut xcfg::scopes::ScopeStack>,
                                                      item_kind: c_uint,
                                                      file_name: StringLenPtr,
                                                      item_name: StringLenPtr,
                                                      pre_xcfg: VecLenPtr<StringLenPtr>,
                                                      post_xcfg: VecLenPtr<StringLenPtr>)
    -> Option<&'stk xcfg::scopes::ScopeConfig> {
    let item_kind = match item_kind {
        ITEM_KIND_FUNCTION => xcfg::scopes::ItemKind::Function,
        ITEM_KIND_STRUCT   => xcfg::scopes::ItemKind::Struct,
        ITEM_KIND_IMPL     => xcfg::scopes::ItemKind::Impl,
        _ => panic!("unknown item kind: {}", item_kind)
    };
    let pre_xcfg = (0..pre_xcfg.len).map(|i| {
        assert!(pre_xcfg.elem_size as usize == mem::size_of::<StringLenPtr>());
        let pre_str = pre_xcfg.get(i as usize);
        serde_yaml::from_slice(pre_str.to_slice())
            .expect(&format!("invalid YAML: '{}'", pre_str.to_str()))
    }).collect::<Vec<_>>();
    let post_xcfg = (0..post_xcfg.len).map(|i| {
        assert!(post_xcfg.elem_size as usize == mem::size_of::<StringLenPtr>());
        let post_str = post_xcfg.get(i as usize);
        serde_yaml::from_slice(post_str.to_slice())
            .expect(&format!("invalid YAML: '{}'", post_str.to_str()))
    }).collect::<Vec<_>>();
    scope_stack.map(|stk| stk.push_item(item_kind,
                                        file_name.to_str(),
                                        item_name.to_str(),
                                        &pre_xcfg, &post_xcfg))
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_stack_pop(scope_stack: Option<&mut xcfg::scopes::ScopeStack>) {
    scope_stack.unwrap().pop();
    // TODO: return something???
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_stack_pop_multi(scope_stack: Option<&mut xcfg::scopes::ScopeStack>,
                                                cnt: c_uint) {
    scope_stack.unwrap().pop_multi(cnt as usize);
    // TODO: return something???
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_stack_last<'stk>(scope_stack: Option<&'stk mut xcfg::scopes::ScopeStack>)
    -> Option<&'stk mut xcfg::scopes::ScopeConfig> {
    scope_stack.map(xcfg::scopes::ScopeStack::last_mut)
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_enabled(scope_config: Option<&xcfg::scopes::ScopeConfig>)
    -> c_uint {
    if scope_config.unwrap().inherited.enabled { 1 } else { 0 }
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_entry_xcheck<'sc>(scope_config: Option<&'sc xcfg::scopes::ScopeConfig>)
    -> Option<&'sc xcfg::XCheckType> {
    scope_config.map(|sc| &sc.inherited.entry)
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_exit_xcheck<'sc>(scope_config: Option<&'sc xcfg::scopes::ScopeConfig>)
    -> Option<&'sc xcfg::XCheckType> {
    scope_config.map(|sc| &sc.inherited.exit)
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_all_args_xcheck<'sc>(scope_config: Option<&'sc xcfg::scopes::ScopeConfig>)
    -> Option<&'sc xcfg::XCheckType> {
    scope_config.map(|sc| &sc.inherited.all_args)
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_ret_xcheck<'sc>(scope_config: Option<&'sc xcfg::scopes::ScopeConfig>)
    -> Option<&'sc xcfg::XCheckType> {
    scope_config.map(|sc| &sc.inherited.ret)
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_ahasher(scope_config: Option<&xcfg::scopes::ScopeConfig>)
    -> StringLenPtr {
    StringLenPtr::from_option_str(&scope_config.unwrap().inherited.ahasher)
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_shasher(scope_config: Option<&xcfg::scopes::ScopeConfig>)
    -> StringLenPtr {
    StringLenPtr::from_option_str(&scope_config.unwrap().inherited.shasher)
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_function_arg<'sc>(scope_config: Option<&'sc xcfg::scopes::ScopeConfig>,
                                                 arg_name: StringLenPtr)
    -> Option<&'sc xcfg::XCheckType> {
    scope_config.and_then(|sc| match sc.item {
        xcfg::scopes::ItemConfig::Function(ref f) => {
            let arg_index = xcfg::FieldIndex::Str(String::from(arg_name.to_str()));
            f.args.get(&arg_index)
        }
        _ => None
    })
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_function_entry_extra(scope_config: Option<&xcfg::scopes::ScopeConfig>)
    -> VecLenPtr<xcfg::ExtraXCheck> {
    match scope_config.unwrap().item {
        xcfg::scopes::ItemConfig::Function(ref f) =>
            VecLenPtr::from_slice(&f.entry_extra),
        _ => Default::default(),
    }
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_function_exit_extra(scope_config: Option<&xcfg::scopes::ScopeConfig>)
    -> VecLenPtr<xcfg::ExtraXCheck> {
    match scope_config.unwrap().item {
        xcfg::scopes::ItemConfig::Function(ref f) =>
            VecLenPtr::from_slice(&f.exit_extra),
        _ => Default::default(),
    }
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_struct_custom_hash(scope_config: Option<&xcfg::scopes::ScopeConfig>)
    -> StringLenPtr {
    match scope_config.unwrap().item {
        xcfg::scopes::ItemConfig::Struct(ref s) =>
            StringLenPtr::from_option_str(&s.custom_hash),
        _ => Default::default(),
    }
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_struct_field_hasher(scope_config: Option<&xcfg::scopes::ScopeConfig>)
    -> StringLenPtr {
    match scope_config.unwrap().item {
        xcfg::scopes::ItemConfig::Struct(ref s) =>
            StringLenPtr::from_option_str(&s.field_hasher),
        _ => Default::default(),
    }
}

#[no_mangle]
pub unsafe extern fn xcfg_scope_struct_field<'sc>(scope_config: Option<&'sc xcfg::scopes::ScopeConfig>,
                                                  field_name: StringLenPtr)
    -> Option<&'sc xcfg::XCheckType> {
    scope_config.and_then(|sc| match sc.item {
        xcfg::scopes::ItemConfig::Struct(ref s) => {
            let field_index = xcfg::FieldIndex::Str(String::from(field_name.to_str()));
            s.fields.get(&field_index)
        }
        _ => None
    })
}
