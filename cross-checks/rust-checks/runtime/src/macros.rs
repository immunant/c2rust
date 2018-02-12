
// FIXME: do we even need this one???
#[macro_export]
macro_rules! cross_check_iter {
    ($iter:expr) => { $crate::xcheck::xcheck($iter) };
}

#[macro_export]
macro_rules! cross_check_raw {
    ($item:expr) => {
        cross_check_raw!(UNKNOWN_TAG, $item)
    };
    ($tag:ident, $item:expr) => {{
        use std::iter::once;
        cross_check_iter!(once(($crate::xcheck::$tag, $item as u64)))
    }};
}

#[macro_export]
macro_rules! cross_check_value {
    ($value:expr) => {
        cross_check_value!(UNKNOWN_TAG, $value)
    };
    ($tag:ident, $value:expr) => {
        cross_check_value!($tag, $value,
                           cross_check_types::DefaultAggHasher,
                           cross_check_types::DefaultSimpleHasher);
    };
    // This form allows the user to pick the hashers, where:
    //   $ahasher == the hasher to use for aggregate/derived values
    //   $shasher == the hasher to use for simple values
    ($tag:ident, $value:expr, $ahasher:ty, $shasher:ty) => {{
        use $crate::hash::CrossCheckHash as XCH;
        if let Some(hash) = XCH::cross_check_hash::<$ahasher, $shasher>(&$value) {
            cross_check_raw!($tag, hash)
        }
    }}
}
