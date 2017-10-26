
#[macro_export]
macro_rules! cross_check_raw {
    ($item:expr) => {
        cross_check_raw!(UNKNOWN_TAG, $item);
    };
    ($tag:ident, $item:expr) => {
        $crate::xcheck::xcheck($crate::xcheck::$tag, $item as u64);
    };
}

#[macro_export]
macro_rules! cross_check_value {
   ($value:expr) => {
       cross_check_value!(UNKNOWN_TAG, $value);
   };
   ($tag:ident, $value:expr) => {
       use $crate::hash::XCheckHash;
       use $crate::hash::jodyhash::JodyHasher;
       use $crate::hash::simple::SimpleHasher;
       $crate::xcheck::xcheck(
           $crate::xcheck::$tag,
           XCheckHash::xcheck_hash::<JodyHasher, SimpleHasher>(&$value));
   };
}
