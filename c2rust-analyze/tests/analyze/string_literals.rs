pub fn inline_desugared_bstr() -> &'static [u8; 0] {
    &[]
}

pub const DESUGARED_BSTR: &'static [u8; 0] = &[];

#[cfg(any())]
pub fn const_desugared_bstr() -> &'static [u8; 0] {
    DESUGARED_BSTR
}

#[cfg(any())]
pub fn bstr() -> &'static [u8; 0] {
    b""
}

#[cfg(any())]
pub fn str() -> &'static str {
    ""
}
