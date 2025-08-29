#[macro_export]
macro_rules! match_or {
    ([$e:expr] $($arm_pat:pat => $arm_body:expr),*; $or:expr) => {
        match $e {
            $( $arm_pat => $arm_body, )*
            _ => $or,
        }
    };
}

#[macro_export]
macro_rules! match_or_else {
    ([$e:expr] $($arm_pat:pat => $arm_body:expr),*; $or_else:expr) => {
        match $e {
            $( $arm_pat => $arm_body, )*
            ref x @ _ => $or_else(x),
        }
    };
}

#[macro_export]
macro_rules! expect {
    ([$e:expr] $arm_pat:pat => $arm_body:expr) => {
        $crate::match_or_else!([$e] $arm_pat => $arm_body;
            |x| panic!("expected {}, got {:?}", stringify!($arm_pat), x))
    };
    ([$e:expr] $($arm_pat:pat => $arm_body:expr),*) => {
        $crate::match_or_else!([$e] $($arm_pat => $arm_body),*;
            |x| panic!("expected one of: {}, got {:?}", stringify!($($arm_pat),*), x))
    };
}

#[macro_export]
macro_rules! unpack {
    ([$e:expr] $enum_:ident :: $variant:ident ( $($arg:ident),* )) => {
        let ($($arg,)*) = $crate::expect!([$e] $enum_::$variant($($arg),*) => ($($arg,)*));
    };
}

#[macro_export]
macro_rules! matches {
    ([$e:expr] $($pat:pat),*) => {
        match $e {
            $( $pat => true, )*
            _ => false,
        }
    };
}

#[macro_export]
#[cfg(feature = "profile")]
macro_rules! profile_start {
    ($msg:expr) => {
        flame::start($msg)
    };
}

#[macro_export]
#[cfg(not(feature = "profile"))]
macro_rules! profile_start {
    ($msg:expr) => {};
}

#[macro_export]
#[cfg(feature = "profile")]
macro_rules! profile_end {
    ($msg:expr) => {
        flame::end($msg)
    };
}

#[macro_export]
#[cfg(not(feature = "profile"))]
macro_rules! profile_end {
    ($msg:expr) => {};
}
