// FIXME: do we even need this one???
#[macro_export]
macro_rules! cross_check_iter {
    ($iter:expr) => {
        $crate::xcheck::xcheck($iter)
    };
}

#[macro_export]
macro_rules! cross_check_raw {
    ($item:expr) => {
        cross_check_raw!(UNKNOWN_TAG, $item)
    };
    ($tag:ident, $item:expr) => {{
        use core::iter::once;
        cross_check_iter!(once(($crate::xcheck::$tag, $item as u64)))
    }};
}

#[macro_export]
macro_rules! cross_check_value {
    ($value:expr) => {
        cross_check_value!(UNKNOWN_TAG, $value)
    };
    ($tag:ident, $value:expr) => {
        cross_check_value!(
            $tag,
            $value,
            $crate::hash::jodyhash::JodyHasher,
            $crate::hash::simple::SimpleHasher
        );
    };
    // This form allows the user to pick the hashers, where:
    //   $ahasher == the hasher to use for aggregate/derived values
    //   $shasher == the hasher to use for simple values
    ($tag:ident, $value:expr, $ahasher:ty, $shasher:ty) => {{
        use $crate::hash::CrossCheckHash as XCH;
        if let Some(hash) = XCH::cross_check_hash::<$ahasher, $shasher>(&$value) {
            cross_check_raw!($tag, hash)
        }
    }};
}

#[macro_export]
macro_rules! __c2rust_impl_union_hash {
    ($hash_ty:ident, Function, $hash_fn:ident, $ahasher:ty, $shasher:ty) => {
        impl $crate::hash::CrossCheckHash for $hash_ty {
            #[inline]
            fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
                where HA: $crate::hash::CrossCheckHasher,
                      HS: $crate::hash::CrossCheckHasher
            {
                $hash_fn::<$ahasher, $shasher>(&self, depth)
            }
        }
    };

    ($hash_ty:ident, Expression, $e:expr) => {
        impl $crate::hash::CrossCheckHash for $hash_ty {
            #[inline]
            fn cross_check_hash_depth<HA, HS>(&self, _depth: usize) -> u64
                where HA: $crate::hash::CrossCheckHasher,
                      HS: $crate::hash::CrossCheckHasher
            {
                $e
            }
        }
    };

    ($hash_ty:ident, Extern, $hash_fn:ident) => {
        impl $crate::hash::CrossCheckHash for $hash_ty {
            #[inline]
            fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
                where HA: $crate::hash::CrossCheckHasher,
                      HS: $crate::hash::CrossCheckHasher
            {
                extern {
                    #[no_mangle]
                    fn $hash_fn(_: *const (), _: usize) -> u64;
                }
                unsafe { $hash_fn(self as *const (), depth) }
            }
        }
    };

    ($hash_ty:ident, Default) => {
        impl $crate::hash::CrossCheckHash for $hash_ty {
            #[inline]
            fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
                where HA: $crate::hash::CrossCheckHasher,
                      HS: $crate::hash::CrossCheckHasher
            {
                if depth == 0 {
                    $crate::hash::LEAF_RECORD_HASH
                } else {
                    $crate::hash::ANY_UNION_HASH
                }
            }
        }
    }
}

#[macro_export]
macro_rules! __c2rust_export_extern_hash {
    ($hash_ty:ident, $hash_fn:ident, $sec_meta:meta, $ahasher:ty, $shasher:ty) => {
        #[no_mangle]
        #[$sec_meta]
        pub unsafe extern "C" fn $hash_fn(x: *mut $hash_ty, depth: usize) -> u64 {
            #[allow(unused_imports)]
            use $crate::hash::CrossCheckHash;
            (*x).cross_check_hash_depth::<$ahasher, $shasher>(depth)
        }
    };
}

#[macro_export]
macro_rules! __c2rust_import_extern_hash {
    ($hash_ty:ident, $hash_fn:ident) => {
        impl $crate::hash::CrossCheckHash for $hash_ty {
            #[inline]
            fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
            where
                HA: $crate::hash::CrossCheckHasher,
                HS: $crate::hash::CrossCheckHasher,
            {
                extern "C" {
                    #[no_mangle]
                    fn $hash_fn(_: *const $hash_ty, _: usize) -> u64;
                }
                unsafe { $hash_fn(self as *const $hash_ty, depth) }
            }
        }
    };
}

#[macro_export]
macro_rules! __c2rust_emit_xcheck {
    ($tag:expr, $val_ident:ident, $val_ref_ident:ident, $ahasher:ty, $shasher:ty $(, $pre:stmt;)*) => {{
        let $val_ref_ident = &$val_ident;
        $($pre)*
        let __c2rust_hash = $crate::hash::CrossCheckHash::cross_check_hash::<$ahasher, $shasher>($val_ref_ident);
        __c2rust_hash.map(|hash| ($tag, hash))
    }}
}
