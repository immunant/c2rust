#![feature(extern_types)]
#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

pub mod repository {
    #[c2rust::header_src = "attrcache.h:1"]
    pub mod attrcache_h {
        extern "C" {
            #[c2rust::src_loc = "1:1"]
            pub type git_attr_cache;
        }
    }

    #[c2rust::header_src = "repository.h:2"]
    pub mod repository_h {
        pub use super::attrcache_h::git_attr_cache;

        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "2:1"]
        pub struct git_repository {
            pub attrcache: *mut git_attr_cache,
        }
    }

    use repository_h::git_repository;
}

pub mod attrcache {
    #[c2rust::header_src = "attrcache.h:1"]
    pub mod attrcache_h {
        #[c2rust::src_loc = "1:1"]
        pub const GIT_ATTR_CONFIG: i32 = 0;
    }

    #[c2rust::header_src = "repository.h:2"]
    pub mod repository_h {
        use super::git_attr_cache;

        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "2:1"]
        pub struct git_repository {
            pub attrcache: *mut git_attr_cache,
        }

        #[c2rust::src_loc = "3:1"]
        pub unsafe extern "C" fn git_repository_attr_cache(
            repo: *mut git_repository,
        ) -> *mut git_attr_cache {
            (*repo).attrcache
        }
    }

    use repository_h::git_repository;

    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct git_attr_cache {
        pub x: i32,
    }

    unsafe extern "C" fn attrcache_source_function(
        repo: *mut git_repository,
    ) -> *mut git_attr_cache {
        repository_h::git_repository_attr_cache(repo)
    }
}

fn main() {}
