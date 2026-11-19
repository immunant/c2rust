#![feature(extern_types, register_tool)]
#![register_tool(c2rust)]

pub mod a {
    #[c2rust::header_src = "api.h:1"]
    pub mod api_h {
        extern "C" {
            #[c2rust::src_loc = "3:9"]
            pub type Ctx;
        }
        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "4:9"]
        pub struct Api {
            pub call: Option<unsafe extern "C" fn(*mut Ctx)>,
        }
    }
    pub use self::api_h::{Api, Ctx};
}

pub mod b {
    #[c2rust::header_src = "bundle.h:1"]
    pub mod bundle_h {
        extern "C" {
            #[c2rust::src_loc = "103:9"]
            pub type Ctx;
        }
        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "104:9"]
        pub struct Api {
            pub call: Option<unsafe extern "C" fn(*mut Ctx)>,
        }
    }
    pub use self::bundle_h::{Api, Ctx};

    #[no_mangle]
    #[c2rust::src_loc = "3:1"]
    pub unsafe extern "C" fn invoke(api: *const Api, ctx: *mut Ctx) {
        (*api).call.expect("non-null function pointer")(ctx);
    }
}

fn main() {}
