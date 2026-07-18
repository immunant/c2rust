#![allow(dead_code)]
#![allow(unused_imports)]

mod stuff {
    pub mod dual {
        pub struct S;
        pub struct S2;
    }
    pub fn dual() {}
}

// An aliased module import: resolving `alias_mod::tgt::S` must traverse the
// module resolution of the `use`.
mod alias_mod {
    pub use crate::stuff::dual as tgt;
}

// `stuff::dual` names both a module and a function, so this import resolves
// in the type and value namespaces at once. Resolving `use_both::dual::S2`
// must traverse the module resolution and ignore the function.
mod use_both {
    pub use crate::stuff::dual;
}

mod valfns {
    pub fn shadow() {}
}

// `shadow` names a value-only import and a sibling module. Resolving
// `tricky::shadow::U` must skip the import (which has no module resolution
// to traverse, and following its function resolution would panic) and
// select the module.
mod tricky {
    pub use crate::valfns::shadow;
    pub mod shadow {
        pub struct U;
    }
}

fn main() {}
