#![feature(c_variadic, extern_types, register_tool)]
#![register_tool(c2rust)]
#![allow(dead_code, unused_imports)]

pub mod first {
    #[c2rust::header_src = "api.h:1"]
    pub mod api_h {
        extern "C" {
            #[c2rust::src_loc = "1:1"]
            pub type CallbackContext;
        }

        #[repr(C)]
        #[c2rust::src_loc = "2:1"]
        pub struct Api {
            pub append: Option<unsafe extern "C" fn(*mut CallbackContext, core::ffi::VaList)>,
        }
    }
}

pub mod extension {
    #[c2rust::header_src = "api.h:1"]
    pub mod api_h {
        extern "C" {
            #[c2rust::src_loc = "1:1"]
            pub type CallbackContext;
        }

        #[repr(C)]
        #[c2rust::src_loc = "2:1"]
        pub struct Api {
            pub append: Option<unsafe extern "C" fn(*mut CallbackContext, core::ffi::VaList)>,
        }
    }

    #[no_mangle]
    pub unsafe extern "C" fn init(_: *const api_h::Api) {}
}

pub mod caller {
    #[c2rust::header_src = "api.h:1"]
    pub mod api_h {
        extern "C" {
            #[c2rust::src_loc = "2:1"]
            pub type Api;

            #[c2rust::src_loc = "3:1"]
            pub fn init(api: *const Api);
        }
    }

    pub unsafe fn call() {
        api_h::init(core::ptr::null::<api_h::Api>());
    }
}

fn main() {}
