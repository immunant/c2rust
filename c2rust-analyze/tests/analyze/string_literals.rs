pub fn inline_desugared_bstr() -> &'static [u8; 0] {
    &[]
}

const DESUGARED_BSTR: &'static [u8; 0] = &[];

#[cfg(any())]
pub fn outline_desugared_bstr() -> &'static [u8; 0] {
    DESUGARED_BSTR
}

pub fn inline_bstr() -> &'static [u8; 0] {
    b""
}

const BSTR: &'static [u8; 0] = b"";

#[cfg(any())]
pub fn outline_bstr() -> &'static [u8; 0] {
    BSTR
}

pub fn inline_str() -> &'static str {
    ""
}

const STR: &'static str = "";

#[cfg(any())]
pub fn outline_str() -> &'static str {
    STR
}

#[cfg(any())]
pub fn named_inline_str() -> &'static str {
    const S: &str = "";
    S
}

#[cfg(any())]
pub fn named_inline_fn_ref() -> fn() -> () {
    fn f() {}
    f
}
