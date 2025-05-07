/// Like [`From`] and [`Into`], but can't `impl` those because of the orphan rule.
pub trait Convert<T> {
    fn convert(self) -> T;
}
