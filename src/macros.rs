macro_rules! match_or {
    ([$e:expr] $($arm_pat:pat => $arm_body:expr),*; $or:expr) => {
        match $e {
            $( $arm_pat => $arm_body, )*
            _ => $or,
        }
    };
}

macro_rules! expect {
    ([$e:expr] $arm_pat:pat => $arm_body:expr) => {
        match_or!([$e] $arm_pat => $arm_body;
            panic!(concat!("expected ", stringify!($arm_pat))))
    };
    ([$e:expr] $($arm_pat:pat => $arm_body:expr),*) => {
        match_or!([$e] $($arm_pat => $arm_body),*;
            panic!(concat!("expected one of: ",
                           stringify!($($arm_pat),*))))
    };
}

macro_rules! unpack {
    ([$e:expr] $enum_:ident :: $variant:ident ( $($arg:ident),* )) => {
        let ($($arg,)*) = expect!([$e] $enum_::$variant($($arg),*) => ($($arg,)*));
    };
}

macro_rules! matches {
    ([$e:expr] $($pat:pat),*) => {
        match $e {
            $( $pat => true, )*
            _ => false,
        }
    };
}
