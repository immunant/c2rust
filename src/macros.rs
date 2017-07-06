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
